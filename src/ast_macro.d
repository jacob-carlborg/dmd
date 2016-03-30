// Compiler implementation of the D programming language
// Copyright (c) 2016 by Digital Mars
// All Rights Reserved
// written by Jacob Carlborg
// http://www.digitalmars.com
// Distributed under the Boost Software License, Version 1.0.
// http://www.boost.org/LICENSE_1_0.txt

module ddmd.ast_macro;

import core.stdc.stdio;

debug
{
    import std.stdio : println = writeln, printfln = writefln;
    import std.string : fromStringz;
}

import ddmd.arraytypes;
import ddmd.ctfeexpr;
import ddmd.dclass;
import ddmd.declaration;
import ddmd.dmodule;
import ddmd.dscope;
import ddmd.expression;
import ddmd.errors;
import ddmd.func;
import ddmd.globals;
import ddmd.id;
import ddmd.identifier;
import ddmd.init;
import ddmd.mtype;
import ddmd.tokens;
import ddmd.visitor;

void toMacroArguments(Loc loc, Scope* sc, Expressions* macroArgs, Expressions* args)
{
    foreach (i, arg ; *args)
    {
        arg = arg.semantic(sc);
        auto result = toMacroAst(loc, arg);

        if (result)
            (*macroArgs)[i] = result;
    }
}

Expression toMacroAst(Loc loc, Expression exp)
{
    extern (C++) scope final class ToMacroAst : Visitor
    {
        alias visit = super.visit;

        Loc loc;
        Expression result;

        this(Loc loc)
        {
            this.loc = loc;
        }

        override void visit(Expression)
        {
            assert(0);
        }

        override void visit(AddExp exp)
        {
            auto left = toMacroAst(loc, exp.e1);
            auto right = toMacroAst(loc, exp.e2);
            auto ctorArgs = createCtorArgs(left, right);
            result = createNewExp(macroAst.types.addExp, ctorArgs);
        }

        override void visit(IntegerExp exp)
        {
            auto ctorArgs = createCtorArgs(exp);
            result = createNewExp(macroAst.types.integerExp, ctorArgs);
        }

        extern(D):

        Expressions* createCtorArgs(Expression[] exps...)
        {
            auto ctorArgs = new Expressions;

            foreach (e ; exps)
                ctorArgs.push(e);

            return ctorArgs;
        }

        NewExp createNewExp(Type type, Expressions* args)
        {
            return new NewExp(loc, null, null, type, args);
        }
    }

    if (!macroAst.initialized)
        macroAst.init(loc);

    scope v = new ToMacroAst(loc);
    exp.accept(v);

    return v.result;
}

Expression fromMacroAst(Loc loc, Scope* sc, MacroDeclaration md, Expression exp)
{
    static bool isAstPackage(ClassDeclaration decl)
    {
        return decl.parent &&
            decl.parent.isModule &&
            decl.parent.parent &&
            decl.parent.parent.isPackage &&
            decl.parent.parent.isPackage.isCorePackage(Id.ast);
    }

    static bool isValidMacroAstNode(ClassDeclaration decl)
    {
        do
        {
            auto ident = decl.ident;

            if (ident == Id.AstNode)
                return isAstPackage(decl);

            decl = decl.baseClass;
        } while (decl);

        return false;
    }

    static ClassReferenceExp getClassRefExp(Expression exp)
    {
        if (exp.op != TOKclassreference)
            return null;

        auto classRef = cast(ClassReferenceExp) exp;
        auto classDecl = classRef.originalClass;

        return isValidMacroAstNode(classDecl) ? classRef : null;
    }

    if (!macroAst.initialized)
        macroAst.init(loc);

    if (auto classRef = getClassRefExp(exp))
        return FromMacroAst(loc, sc, md).visit(classRef);
    else
    {
        md.error(loc, "needs to return a value of type %s, not (%s) of type %s",
            macroAst.types.astNode.toChars(), exp.toChars(),
            exp.type.toPrettyChars());

        return new ErrorExp();
    }
}

private:

__gshared MacroAst macroAst;

struct MacroAst
{
    static struct Modules
    {
        Module astNode;
        Module expression;

        void init()
        {
            astNode = findModule(Id.ast_node);
            expression = findModule(Id.expression);
        }

        Module findModule(Identifier ident)
        {
            foreach (mod ; Module.amodules)
            {
                if (mod.isCoreModule(Id.ast, ident))
                    return mod;
            }

            assert(0, "The identifier '" ~ ident.toChars.fromStringz ~
                "' is not a module in the package 'core.ast'");
        }
    }

    static struct Types
    {
        Type astNode;
        Type addExp;
        Type integerExp;

        void init(Loc loc, /*const*/ ref Modules modules)
        {
            astNode = findType(loc, modules.astNode, Id.AstNode);
            addExp = findType(loc, modules.expression, Id.AddExp);
            integerExp = findType(loc, modules.expression, Id.IntegerExp);
        }

        Type findType(Loc loc, Module mod, Identifier ident)
        {
            auto exp = mod.search(loc, ident);
            assert(exp, "Could not find the type '" ~
                ident.toChars.fromStringz ~ "' in module '" ~
                mod.toChars.fromStringz ~ "'");

            auto type = exp.getType;
            assert(type, "The identifier '" ~ ident.toChars.fromStringz ~
                " in module '" ~ mod.toChars.fromStringz ~ "' is not a type");

            return type;
        }
    }

    Modules modules;
    Types types;
    bool initialized;

    void init(Loc loc)
    {
        modules.init();
        types.init(loc, modules);
        initialized = true;
    }
}

struct FromMacroAst
{
    private alias Dispatcher = Expression delegate(ClassReferenceExp);

    private enum dispatchTable = [
        NodeType.addExp: &visitAddExp,
        NodeType.integerExp: &visitIntegerExp,
        NodeType.varDeclaration: &visitVarDeclaration,
        NodeType.basicType: &visitBasicType
    ];

    // This enum needs to match the one in the core.ast.ast_node
    private enum NodeType : short
    {
        astNode,

        declaration,
        varDeclaration,

        expression,
        binExp,
        addExp,
        integerExp,

        initializer,
        symbol,

        type,
        basicType
    }

    Loc loc;
    Scope* sc;
    MacroDeclaration md;

    Expression visit(ClassReferenceExp exp)
    {
        auto nodeType = extractNodeType(exp);
        return dispatch(nodeType, exp);
    }

private:

    Expression visitAddExp(ClassReferenceExp exp)
    {
        auto e1 = getFieldValue(exp, Id.left);
        auto e2 = getFieldValue(exp, Id.right);

        e1 = fromMacroAst(e1);
        e2 = fromMacroAst(e2);

        return new AddExp(loc, e1, e2);
    }

    Expression visitIntegerExp(ClassReferenceExp exp)
    {
        auto value = getFieldValue(exp, Id.value);
        assert(isValidIntegerExp(value));

        return value;
    }

    Expression visitVarDeclaration(ClassReferenceExp exp)
    {
        auto identExp = getFieldValue(exp, Id.ident);
        auto ident = expToIdent(identExp);

        auto typeExp = getFieldValue(exp, Id.type);
        auto type = expToType(fromMacroAst(typeExp));

        auto initExp = getFieldValue(exp, Id.initializer);
        initExp = initExp.op == TOKnull ? initExp : fromMacroAst(initExp);
        auto init = expToInit(initExp);

        auto var = new VarDeclaration(loc, type, ident, init);
        var.linkage = LINKd;

        return new DeclarationExp(loc, var);
    }

    Expression visitBasicType(ClassReferenceExp exp)
    {
        auto typeKind = getFieldValue(exp, Id.typeKind);
        auto ty = expToTY(typeKind);
        Type type;

        switch(ty)
        {
            case Tint32: type = Type.tint32; break;
            default:
                printf("visitTypeBasic ty=%s\n", ty);
                assert(0);
        }

        return new TypeExp(loc, type);
    }

    NodeType extractNodeType(ClassReferenceExp exp)
    {
        auto cd = exp.originalClass;
        auto e = cd.symtab.lookup(Id.nodeType);
        assert(e !is null, "Could not find 'nodeType' in the AST node '" ~
            cd.type.toChars.fromStringz ~ "'");

        auto var = e.isVarDeclaration;
        assert(var !is null, "'nodeType' is not a variable declaration " ~
            "in the AST node '" ~ cd.type.toChars.fromStringz ~ "'");

        auto init = var._init;
        assert(init !is null, "'nodeType' does not have an initializer " ~
            "in the AST node '" ~ cd.type.toChars.fromStringz ~ "'");

        auto expInit = init.isExpInitializer;
        assert(expInit !is null, "The initializer of 'nodeType' is not an " ~
            "expression in the AST node '" ~ cd.type.toChars.fromStringz ~ "'");

        assert(expInit.exp.op == TOKint64, "The type of 'nodeType' is not " ~
            "'long' in the AST node '" ~ cd.type.toChars.fromStringz ~ "'");

        auto value = (cast(IntegerExp) expInit.exp).value;
        assert(value >= 0 && value <= NodeType.max);

        return cast(NodeType) value;
    }

    Expression dispatch(NodeType nodeType, ClassReferenceExp exp)
    {
        if (auto func = nodeType in dispatchTable)
            return buildDispatcher(*func)(exp);
        else
        {
            printf("Could not find the node type %d in the dispatch table\n", nodeType);
            assert(0);
        }
    }

    Dispatcher buildDispatcher(Expression function(ClassReferenceExp) func)
    {
        Expression delegate(ClassReferenceExp) dispatcher;
        dispatcher.ptr = cast(void*) &this;
        dispatcher.funcptr = func;

        return dispatcher;
    }

    Expression fromMacroAst(Expression exp)
    {
        return .fromMacroAst(loc, sc, md, exp);
    }

    Expression getFieldValue(ClassReferenceExp exp, Identifier ident)
    {
        auto index = getFieldIndex(exp, ident);
        assert(index != -1, "Cannot find a field with the name '" ~
            ident.toString ~ "' in the node type '" ~
            exp.originalClass.type.toChars.fromStringz ~ "'");

        return getFieldValue(exp, index);
    }

    Expression getFieldValue(ClassReferenceExp exp, size_t index)
    {
        auto value = (*exp.value.elements)[index];
        assert(value);

        return value;
    }

    int getFieldIndex(ClassReferenceExp exp, Identifier ident)
    {
        auto cd = exp.originalClass();
        auto elements = exp.value.elements;
        size_t fieldsSoFar = 0;

        foreach (i ; 0 .. elements.dim)
        {
            while (i - fieldsSoFar >= cd.fields.dim)
            {
                fieldsSoFar += cd.fields.dim;
                cd = cd.baseClass;
            }

            auto var = cd.fields[i - fieldsSoFar];

            if (var.ident == ident)
                return cast(int)(elements.dim - fieldsSoFar - cd.fields.dim + (i - fieldsSoFar));
        }

        return -1;
    }

    bool isValidIntegerExp(Expression exp)
    {
        switch(exp.op)
        {
            case TOKint8:
            case TOKuns8:
            case TOKint16:
            case TOKuns16:
            case TOKint32:
            case TOKuns32:
            case TOKint64:
            case TOKuns64:
            case TOKint128:
            case TOKuns128:
                return true;
            default: return false;
        }
    }

    Identifier expToIdent(Expression exp)
    {
        assert(exp.op == TOKstring);
        auto stringExp = cast(StringExp) exp;
        auto name = stringExp.peekSlice();

        return Identifier.idPool(name);
    }

    ulong epxToUlong(Expression exp)
    {
        assert(isValidIntegerExp(exp));
        auto intExp = cast(IntegerExp) exp;
        return intExp.toInteger();
    }

    TY expToTY(Expression exp)
    {
        auto value = epxToUlong(exp);
        assert(value >= 0 && value < TMAX);

        return cast(TY) value;
    }

    Type expToType(Expression exp)
    {
        assert(exp.op == TOKtype);
        return exp.type;
    }

    Initializer expToInit(Expression exp)
    {
        // return new ExpInitializer(loc, exp);
        return exp.op == TOKnull ? null : new ExpInitializer(loc, exp);
    }
}
