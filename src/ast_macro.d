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
    import std.stdio : println = writeln, printfln = writefln, print = write;
    import std.string : fromStringz, toStringz;
    import std.format : format;
    import std.conv : to;
}

import ddmd.root.array;

import ddmd.arraytypes;
import ddmd.ctfeexpr;
import ddmd.dclass;
import ddmd.declaration;
import ddmd.denum;
import ddmd.dmodule;
import ddmd.dscope;
import ddmd.dsymbol;
import ddmd.expression;
import ddmd.errors;
import ddmd.func;
import ddmd.globals;
import ddmd.id;
import ddmd.identifier;
import ddmd.init;
import ddmd.mtype;
import ddmd.statement;
import ddmd.statementsem;
import ddmd.tokens;
import ddmd.visitor;

void toMacroArguments(Loc loc, Scope* sc, Expressions* macroArgs, Expressions* args)
{
    foreach (i, arg ; *args)
    {
        arg = arg.semantic(sc);

        if (arg)
            printAst(arg);

        auto result = toMacroAst(loc, arg, true);

        if (result)
            (*macroArgs)[i] = result;
    }
}

__gshared Expression[void*] visited;

Expression toMacroAst(T)(Loc loc, T exp, bool topLevel = false)
{
    extern (C++) scope final class ToMacroAst : Visitor
    {
        alias visit = super.visit;

        Loc loc;
        Expression result;
        bool topLevel;

        this(Loc loc, bool topLevel)
        {
            this.loc = loc;
            this.topLevel = topLevel;
        }

        override void visit(Expression exp)
        {
            assert(0, format("visit method not implemented for '%s' of type " ~
                "'%s' op=%s", exp.toChars.fromStringz,
                exp.type.toChars.fromStringz, exp.op));
        }

        override void visit(AddExp exp)
        {
            auto left = toMacroAst(loc, exp.e1);
            auto right = toMacroAst(loc, exp.e2);
            auto ctorArgs = createCtorArgs(left, right);
            result = createNewExp(macroAst.types.addExp, ctorArgs);
        }

        override void visit(BlitExp exp)
        {
            __gshared static bool[void*] visited;

            if (cast(void*) exp in visited)
            {
                if (!result)
                    result = new NewExp(loc, null, null, macroAst.types.blitExpression, null);
                return;
            }

            visited[cast(void*) exp] = true;

            auto left = toMacroAst(loc, exp.e1);
            NewExp foobar;

            if (left.op == TOKnew)
            {
                auto n = cast(NewExp) left;

                if (n.arguments.dim >= 0)
                {
                    auto a = (*n.arguments)[0];

                    if (a.op == TOKnew)
                    {
                        auto b = cast(NewExp) a;

                        if (b.arguments.dim >= 0)
                        {
                            auto c = (*b.arguments)[0];
                            if (c.op == TOKnew)
                            {
                                auto d = cast(NewExp) c;

                                if (d.arguments.dim >= 0)
                                {
                                    auto e = (*d.arguments)[0];
                                    if (e.op == TOKnew)
                                    {
                                        auto f = cast(NewExp) e;
                                        if (f.arguments is null)
                                            foobar = f;
                                    }
                                }
                            }
                        }
                    }
                }
            }

            auto right = toMacroAst(loc, exp.e2);
            auto ctorArgs = createCtorArgs(left, right);

            result = createNewExp(macroAst.types.blitExpression, ctorArgs);

            if (foobar)
                foobar.arguments = ctorArgs;
        }

        override void visit(CallExp exp)
        {
            auto decl = toMacroAst(exp.f);
            auto e1 = toMacroAst(exp.e1);

            auto arrayType = macroAst.types.expression.arrayOf;
            auto arguments = toArrayLiteral(exp.arguments, arrayType);

            auto ctorArgs = createCtorArgs(e1, decl, arguments);

            result = createNewExp(macroAst.types.callExpression, ctorArgs);
        }

        override void visit(DeclarationExp exp)
        {
            auto decl = toMacroAst(loc, exp.declaration);
            auto ctorArgs = createCtorArgs(decl);

            result = createNewExp(macroAst.types.declarationExpression, ctorArgs);
        }

        override void visit(IntegerExp exp)
        {
            auto ctorArgs = createCtorArgs(exp);
            result = createNewExp(macroAst.types.integerExp, ctorArgs);
        }

        override void visit(FuncExp exp)
        {
            if (topLevel)
            {
                auto body_ = toMacroAst(loc, exp.fd.fbody);
                auto ctorArgs = createCtorArgs(body_);

                result = createNewExp(macroAst.types.compoundStatement, ctorArgs);
            }
            else
                assert(0);
        }

        override void visit(NullExp e)
        {
            result = e;
        }

        override void visit(StringExp exp)
        {
            auto ctorArgs = createCtorArgs(exp);
            result = createNewExp(macroAst.types.stringExpression, ctorArgs);
        }

        override void visit(VarExp exp)
        {
            auto decl = toMacroAst(loc, exp.var);
            auto ctorArgs = createCtorArgs(decl);

            result = createNewExp(macroAst.types.variableExpression, ctorArgs);
        }

        override void visit(Declaration d)
        {
            assert(0, "visit method not implemented for declaration '" ~
                d.kind.fromStringz ~ "'");
        }

        override void visit(EnumDeclaration d)
        {
            auto ident = toMacroAst(d.ident);
            auto memberType = toMacroAst(d.memtype);
            auto ctorArgs = createCtorArgs(ident, memberType);

            result = createNewExp(macroAst.types.enumDeclaration, ctorArgs);
        }

        override void visit(FuncDeclaration fd)
        {
            auto ident = fd.ident ? toMacroAst(fd.ident) : nullNode;
            auto type = fd.type ? toMacroAst(fd.type) : nullNode;
            auto body_ = fd.fbody ? toMacroAst(fd.fbody) : nullNode;
            auto ctorArgs = createCtorArgs(ident, type, body_);

            result = createNewExp(macroAst.types.functionDeclaration, ctorArgs);
        }

        override void visit(FuncLiteralDeclaration fd)
        {
            (cast(FuncDeclaration) fd).accept(this);
        }

        override void visit(VarDeclaration var)
        {
            auto ident = toMacroAst(loc, var.ident);
            auto type = toMacroAst(loc, var.type);
            auto init = toMacroAst(loc, var._init);
            auto ctorArgs = createCtorArgs(ident, type, init);

            result = createNewExp(macroAst.types.variableDeclaration, ctorArgs);
        }

        override void visit(ExpInitializer ei)
        {
            auto exp = toMacroAst(loc, ei.exp);
            auto ctorArgs = createCtorArgs(exp);

            result = createNewExp(macroAst.types.expressionInitializer, ctorArgs);
        }

        override void visit(Statement s)
        {
            assert(0, "visit method not implemented for '" ~
                s.toChars.fromStringz ~ "'");
        }

        override void visit(CompoundStatement cs)
        {
            auto arrayType = macroAst.types.statement.arrayOf;
            auto statements = toArrayLiteral(cs.statements, arrayType);
            auto ctorArgs = createCtorArgs(statements);

            result = createNewExp(macroAst.types.compoundStatement, ctorArgs);
        }

        override void visit(ExpStatement es)
        {
            auto exp = toMacroAst(loc, es.exp);
            auto ctorArgs = createCtorArgs(exp);

            result = createNewExp(macroAst.types.expressionStatement, ctorArgs);
        }

        override void visit(ImportStatement ip)
        {
            auto arrayType = macroAst.types.symbol.arrayOf;
            auto imports = toArrayLiteral(ip.imports, arrayType);
            auto ctorArgs = createCtorArgs(imports);

            result = createNewExp(macroAst.types.importStatement, ctorArgs);
        }


        override void visit(Type t)
        {
            assert(false, "visit method not implemented for type '" ~
                t.kind.fromStringz ~ "'");
        }

        override void visit(TypeBasic t)
        {
            auto exp = macroAst.typeKinds[t.ty];
            auto ctorArgs = createCtorArgs(exp);

            result = createNewExp(macroAst.types.basicType, ctorArgs);
        }

        override void visit(TypeArray t)
        {
            auto next = toMacroAst(t.next);
            auto ctorArgs = createCtorArgs(next);

            result = createNewExp(macroAst.types.arrayType, ctorArgs);
        }

        override void visit(TypeEnum t)
        {
            auto decl = toMacroAst(t.sym);
            auto ctorArgs = createCtorArgs(decl);

            result = createNewExp(macroAst.types.enumType, ctorArgs);
        }

        override void visit(TypeFunction t)
        {
            auto arrayType = macroAst.types.parameter.arrayOf;
            auto parameters = toArrayLiteral(t.parameters, arrayType);

            auto returnType = toMacroAst(t.next);
            auto variadic = macroAst.variadicTypes[t.varargs];
            auto linkage = macroAst.linkages[t.linkage];

            auto ctorArgs = createCtorArgs(parameters, returnType, variadic, linkage);
            result = createNewExp(macroAst.types.functionType, ctorArgs);
        }

        override void visit(TypePointer t)
        {
            auto next = toMacroAst(t.next);
            auto ctorArgs = createCtorArgs(next);

            result = createNewExp(macroAst.types.pointerType, ctorArgs);
        }

        override void visit(TypeTuple t)
        {
            auto arrayType = macroAst.types.parameter.arrayOf;
            auto arguments = toArrayLiteral(t.arguments, arrayType);

            auto ctorArgs = createCtorArgs(arguments);
            result = createNewExp(macroAst.types.tupleType, ctorArgs);
        }

        override void visit(Dsymbol s)
        {
            auto ident = s.ident ? toMacroAst(s.ident) : nullNode;

            auto ctorArgs = createCtorArgs(ident);
            result = createNewExp(macroAst.types.symbol, ctorArgs);
        }

        override void visit(ScopeDsymbol s)
        {
            auto ident = s.ident ? toMacroAst(s.ident) : nullNode;

            auto ctorArgs = createCtorArgs(ident);
            result = createNewExp(macroAst.types.scopeSymbol, ctorArgs);
        }

        override void visit(Identifier i)
        {
            result = i ? new StringExp(loc, i.toString()) : nullNode;
        }

        override void visit(Parameter p)
        {
            auto storageClass = new IntegerExp(loc, p.storageClass, Type.tuns64);

            auto type = toMacroAst(p.type);
            auto ident = p.ident ? toMacroAst(p.ident) : nullNode;
            auto defaultArgument = p.defaultArg ? toMacroAst(p.defaultArg) : nullNode;

            auto ctorArgs = createCtorArgs(storageClass, type, ident, defaultArgument);
            result = createNewExp(macroAst.types.parameter, ctorArgs);
        }

        extern(D):

        Expressions* createCtorArgs(Expression[] exps...)
        in
        {
            foreach (e ; exps)
                assert(e);
        }
        body
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

        ArrayLiteralExp toArrayLiteral(T)(Array!(T)* array, Type type)
        {
            auto expressions = new Expressions;
            expressions.reserve(array.dim);

            foreach (i ; 0 .. array.dim)
                expressions.push(toMacroAst(loc, (*array)[i]));

            auto exp = new ArrayLiteralExp(loc, expressions);
            exp.type = type;
            exp.ownedByCtfe = OWNEDctfe;

            return exp;
        }

        Expression toMacroAst(T)(T exp)
        {
            return .toMacroAst(loc, exp);
        }

        Expression toMacroAst(T)(Loc loc, T exp)
        {
            return .toMacroAst(loc, exp);
        }

        Expression nullNode()
        {
            return toMacroAst(new NullExp(loc));
        }
    }

    if (!macroAst.initialized)
        macroAst.init(loc);

    scope v = new ToMacroAst(loc, topLevel);
    assert(exp);
    exp.accept(v);
    assert(v.result);
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
        assert(exp.op == TOKclassreference);

        auto classRef = cast(ClassReferenceExp) exp;
        auto classDecl = classRef.originalClass;

        return isValidMacroAstNode(classDecl) ? classRef : null;
    }

    if (!macroAst.initialized)
        macroAst.init(loc);

    switch (exp.op)
    {
        case TOKclassreference:
            auto classRef = getClassRefExp(exp);
            auto invocation = new MacroInvocation(loc, sc, md);
            return FromMacroAst(invocation).visit(classRef);

        case TOKarrayliteral:
            auto invocation = new MacroInvocation(loc, sc, md);
            return FromMacroAst(invocation).visit(cast(ArrayLiteralExp) exp);

        case TOKnull:
            return null;

        default:
        println(exp.op);
            md.error(loc, "needs to return a value of type %s, not (%s) of type %s",
                macroAst.types.astNode.toChars(), exp.toChars(),
                exp.type.toPrettyChars());

            return new ErrorExp();
    }
}

private __gshared MacroAst macroAst;

extern(C++) final class MacroInvocation : ScopeDsymbol
{
    private __gshared static size_t globalIdSuffix = 0;

    Loc loc;
    Scope* sc;
    MacroDeclaration macroDeclaration;
    MacroInvocation instance;
    ScopeDsymbol argsym;
    Type type;

    private size_t idSuffix;

    this(Loc loc, Scope* sc, MacroDeclaration macroDeclaration)
    {
        this.loc = loc;
        this.sc = sc;
        this.macroDeclaration = macroDeclaration;
        members = new Dsymbols;
        idSuffix = globalIdSuffix++;
    }

    extern (D) Identifier generateId(const(char)[] prefix)
    {
        return Identifier.generateId(prefix.ptr, prefix.length, idSuffix);
    }

    void addMember(Dsymbol member)
    {
        members.push(member);
    }

    override void semantic(Scope* sc)
    {
        static if (LOGSEMANTIC)
            printf("MacroInvocation::semantic('%s')\n", toChars());

        if (type)
            return;

        printf("dim=%d\n", members.dim);
        foreach (i ; 0 .. members.dim)
            (*members)[i].semantic(sc);

        type = Type.tvoid;

        // de.semantic(sc);
        // return de;

        // Scope* scx = null;
        //
        // if (_scope)
        // {
        //     sc = _scope;
        //     scx = _scope; // save so we don't make redundant copies
        //     _scope = null;
        // }
        //
        // instance = this;
        // parent = sc.parent;
        // symtab = new DsymbolTable();
        //
        // for (Scope* sce = sc; 1; sce = sce.enclosing)
        // {
        //     ScopeDsymbol sds = sce.scopesym;
        //     if (sds)
        //     {
        //         sds.importScope(this, Prot(PROTpublic));
        //         break;
        //     }
        // }
        //
        // Scope* scy = sc.push(this);
        // scy.parent = this;
        //
        // argsym = new ScopeDsymbol();
        // argsym.parent = scy.parent;
        // Scope* argscope = scy.push(argsym);
        //
        // var.addMember(argscope, this);
        // Scope* sc2 = argscope.push(this);
        // var.setScope(sc2);
        // var.importAll(sc2);
        // var.semantic(sc2);
        //
        // sc2.pop();
        // argscope.pop();
        // scy.pop();
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

extern (C++) abstract class AstMacroResultExp : Expression
{
    extern (D) this(Loc loc, int size)
    {
        super(loc, TOKast_macro_result_exp, size);
    }

    abstract override Expression semantic(Scope* sc);

    override void accept(Visitor v)
    {
        v.visit(this);
    }

    AstMacroResultInvocationExp isInvocation()
    {
        return null;
    }

    AstMacroResultStatementExp isStatement()
    {
        return null;
    }
}

extern (C++) final class AstMacroResultInvocationExp : AstMacroResultExp
{
    MacroInvocation invocation;

    extern (D) this(Loc loc, MacroInvocation invocation)
    {
        super(loc, __traits(classInstanceSize, AstMacroResultInvocationExp));
        this.invocation = invocation;
    }

    override Expression semantic(Scope* sc)
    {
        static if (LOGSEMANTIC)
            printf("AstMacroResultExp::semantic('%s')\n", toChars());

        if (type)
            return this;

        invocation.semantic(sc);
        type = Type.tvoid;

        return this;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }

    override AstMacroResultInvocationExp isInvocation()
    {
        return this;
    }
}

extern (C++) final class AstMacroResultStatementExp : AstMacroResultExp
{
    Statement statement;

    extern (D) this(Loc loc, Statement statement)
    {
        super(loc, __traits(classInstanceSize, AstMacroResultStatementExp));
        this.statement = statement;
    }

    override Expression semantic(Scope* sc)
    {
        static if (LOGSEMANTIC)
            printf("AstMacroResultStatementExp::semantic('%s')\n", toChars());

        if (type)
            return this;

        statement.semantic(sc);
        type = Type.tvoid;

        return this;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }

    override AstMacroResultStatementExp isStatement()
    {
        return this;
    }
}

private:

struct MacroAst
{
    static struct Modules
    {
        Module astNode;
        Module declaration;
        Module expression;
        Module initializer;
        Module statement;
        Module symbol;
        Module type;
            Module util;

        void init()
        {
            astNode = findModule(Id.ast_node);
            declaration = findModule(Id.declaration);
            expression = findModule(Id.expression);
            initializer = findModule(Id.initializer);
            statement = findModule(Id.statement);
            symbol = findModule(Id.symbol);
            type = findModule(Id.type);
            util = findModule(Id.util);
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
        Type symbol;
        Type scopeSymbol;

        Type enumDeclaration;
        Type functionDeclaration;
        Type variableDeclaration;

        Type expression;
        Type addExp;
        Type assignExpression;
        Type blitExpression;
        Type callExpression;
        Type declarationExpression;
        Type integerExp;
        Type stringExpression;
        Type symbolExpression;
        Type variableExpression;

        Type expressionInitializer;

        Type compoundStatement;
        Type expressionStatement;
        Type importStatement;
        Type statement;

        Type basicType;
        Type arrayType;
        Type enumType;
        Type functionType;
        Type pointerType;
        Type tupleType;

        Type parameter;

        void init(Loc loc, /*const*/ ref Modules modules)
        {
            astNode = findType(loc, modules.astNode, Id.AstNode);
            symbol = findType(loc, modules.symbol, Id.Symbol);
            scopeSymbol = findType(loc, modules.symbol, Id.ScopeSymbol);

            enumDeclaration = findType(loc, modules.declaration, Id.EnumDeclaration);
            variableDeclaration = findType(loc, modules.declaration, Id.VarDeclaration);
            functionDeclaration = findType(loc, modules.declaration, Id.FunctionDeclaration);

            expression = findType(loc, modules.expression, Id.Expression);
            addExp = findType(loc, modules.expression, Id.AddExp);
            assignExpression = findType(loc, modules.expression, Id.AssignExpression);
            blitExpression = findType(loc, modules.expression, Id.BlitExpression);
            callExpression = findType(loc, modules.expression, Id.CallExpression);
            declarationExpression = findType(loc, modules.expression, Id.DeclarationExpression);
            integerExp = findType(loc, modules.expression, Id.IntegerExp);
            stringExpression = findType(loc, modules.expression, Id.StringExpression);
            symbolExpression = findType(loc, modules.expression, Id.SymbolExpression);
            variableExpression = findType(loc, modules.expression, Id.VariableExpression);

            expressionInitializer = findType(loc, modules.initializer, Id.ExpressionInitializer);

            compoundStatement = findType(loc, modules.statement, Id.CompoundStatement);
            expressionStatement = findType(loc, modules.statement, Id.ExpressionStatement);
            importStatement = findType(loc, modules.statement, Id.ImportStatement);
            statement = findType(loc, modules.statement, Id.Statement);

            basicType = findType(loc, modules.type, Id.BasicType);
            arrayType = findType(loc, modules.type, Id.ArrayType);
            enumType = findType(loc, modules.type, Id.EnumType);
            functionType = findType(loc, modules.type, Id.FunctionType);
            pointerType = findType(loc, modules.type, Id.PointerType);
            tupleType = findType(loc, modules.type, Id.TupleType);

            parameter = findType(loc, modules.type, Id.Parameter);
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

    static struct TypeKinds
    {
        private
        {
            DotIdExp[TY] typeKindMap;
            Loc loc;
            Module mod;
            EnumDeclaration typeKind;
        }

        this(Loc loc, /*const*/ ref Modules modules)
        {
            this.loc = loc;
            this.mod = modules.type;
            typeKind = findTypeKind();

            typeKindMap[Tenum] = find(Id.enum_);
            typeKindMap[Tint32] = find(Id.int32);
            typeKindMap[Tvoid] = find(Id.void_);
            typeKindMap[Tchar] = find(Id.char_);
        }

        DotIdExp opIndex(TY ty)
        {
            return typeKindMap[ty];
        }

    private:

        EnumDeclaration findTypeKind()
        {
            auto ident = Id.TypeKind;
            auto symbol = mod.search(loc, ident);
            assert(symbol, "Could not find the enum declaration '" ~
                ident.toChars.fromStringz ~ "' in module '" ~
                symbol.toChars.fromStringz ~ "'");

            auto declaration = symbol.isEnumDeclaration;
            assert(declaration, "The identifier '" ~ ident.toChars.fromStringz ~
                " in module '" ~ symbol.toChars.fromStringz ~ "' is not an enum declaration");

            return declaration;
        }

        DotIdExp find(Identifier ident)
        {
            auto symbol = typeKind.search(loc, ident);
            assert(symbol, "Could not find the enum member '" ~
                ident.toChars.fromStringz ~ "' in module '" ~
                typeKind.toChars.fromStringz ~ "'");

            auto member = symbol.isEnumMember;
            assert(member, "The identifier '" ~ ident.toChars.fromStringz ~
                " in module '" ~ typeKind.toChars.fromStringz ~ "' is not a enum member");

            auto identExp = new IdentifierExp(loc, typeKind.ident);
            return new DotIdExp(loc, identExp, member.ident);
        }
    }

    static struct EnumProjection(T)
    {
        private
        {
            DotIdExp[T] enumMap;
            Loc loc;
            Module mod;
            EnumDeclaration enumDeclaration;
        }

        this(Loc loc, Module mod, Identifier enumIdent,
            Identifier[T] memberMap)
        {
            this.loc = loc;
            this.mod = mod;
            enumDeclaration = findEnumDeclaration(enumIdent);

            foreach (internalValue, ident ; memberMap)
                enumMap[internalValue] = findEnumMember(ident);
        }

        DotIdExp opIndex(T key)
        {
            return enumMap[key];
        }

    private:

        EnumDeclaration findEnumDeclaration(Identifier ident)
        {
            auto symbol = findSymbol(mod, ident, "declaration");
            auto declaration = symbol.isEnumDeclaration;
            assert(declaration, "The identifier '" ~ ident.toChars.fromStringz ~
                " in module '" ~ symbol.toChars.fromStringz ~ "' is not an enum declaration");

            return declaration;
        }

        DotIdExp findEnumMember(Identifier ident)
        {
            auto symbol = findSymbol(enumDeclaration, ident, "member");

            auto member = symbol.isEnumMember;
            assert(member, "The identifier '" ~ ident.toChars.fromStringz ~
                " in module '" ~ enumDeclaration.toChars.fromStringz ~ "' is not a enum member");

            auto identExp = new IdentifierExp(loc, enumDeclaration.ident);
            return new DotIdExp(loc, identExp, member.ident);
        }

        Dsymbol findSymbol(ScopeDsymbol parent, Identifier ident, string kind)
        {
            auto symbol = parent.search(loc, ident);
            assert(symbol, "Could not find the enum " ~ kind ~ " '" ~
                ident.toChars.fromStringz ~ "' in module '" ~
                parent.toChars.fromStringz ~ "'");

            return symbol;
        }
    }

    Modules modules;
    Types types;
    EnumProjection!TY typeKinds;
    EnumProjection!int variadicTypes;
    EnumProjection!LINK linkages;
    bool initialized;

    void init(Loc loc)
    {
        modules.init();
        types.init(loc, modules);

        typeKinds = EnumProjection!TY(loc, modules.type, Id.TypeKind, [
            Tenum: Id.enum_,
            Tint32: Id.int32,
            Tvoid: Id.void_,
            Tchar: Id.char_
        ]);

        variadicTypes = EnumProjection!int(loc, modules.util, Id.VariadicType, [
            0: Id.nonVariadic,
            1: Id.untyped,
            2: Id.typed
        ]);

        linkages = EnumProjection!LINK(loc, modules.util, Id.Linkage, [
            LINK.def: Id.default_,
            LINK.d: Id.d,
            LINK.c: Id.c,
            LINK.cpp: Id.cpp,
            LINK.windows: Id.windows,
            LINK.pascal: Id.pascal,
            LINK.objc: Id.objectiveC
        ]);

        initialized = true;
    }
}

struct FromMacroAst
{
    private alias Dispatcher = Expression delegate(ClassReferenceExp);

    private enum dispatchTable = [
        NodeType.addExp: &visitAddExp,
        NodeType.arrayLiteralExpression: &visitArrayLiteralExpression,
        NodeType.callExpression: &visitCallExpression,
        NodeType.integerExp: &visitIntegerExp,
        NodeType.stringExpression: &visitStringExpression,
        NodeType.functionDeclaration: &visitFunctionDeclaration,
        NodeType.varDeclaration: &visitVarDeclaration,
        NodeType.basicType: &visitBasicType,
        NodeType.functionType: &visitFunctionType,
        NodeType.variableExpression: &visitVariableExpression,
        NodeType.enumType: &visitEnumType,
        NodeType.enumDeclaration: &visitEnumDeclaration,
        NodeType.compoundStatement: &visitCompoundStatement,
        NodeType.expressionStatement: &visitExpressionStatement
    ];

    // This enum needs to match the one in the core.ast.ast_node
    private enum NodeType : short
    {
        astNode,

        declaration,
        enumDeclaration,
        varDeclaration,
        functionDeclaration,

        expression,
        arrayLiteralExpression,
        assignExpression,
        addExp,
        binExp,
        blitExpression,
        callExpression,
        declarationExpression,
        integerExp,
        stringExpression,
        symbolExpression,
        unaryExpression,
        variableExpression,

        initializer,
        expressionInitializer,

        symbol,
        scopeSymbol,

        statement,
        compoundStatement,
        expressionStatement,
        importStatement,

        type,
        arrayType,
        basicType,
        enumType,
        functionType,
        nextType,
        pointerType,
        tupleType,

        parameter
    }

    Loc loc;
    Scope* sc;
    MacroDeclaration md;
    MacroInvocation invocation;

    this(MacroInvocation invocation)
    {
        this.invocation = invocation;
        this.loc = invocation.loc;
        this.sc = invocation.sc;
        this.md = invocation.macroDeclaration;
    }

    Expression visit(ClassReferenceExp exp)
    {
        auto nodeType = extractNodeType(exp);
        return dispatch(nodeType, exp);
    }

    Expression visit(ArrayLiteralExp exp)
    {
        auto elements = new Expressions;
        elements.reserve(exp.elements.dim);

        foreach (e ; *exp.elements)
            elements.push(fromMacroAst(e));

        return new ArrayLiteralExp(loc, elements);
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

    Expression visitArrayLiteralExpression(ClassReferenceExp exp)
    {
        auto elementsExp = getFieldValue(exp, Id.elements);
        assert(0);
    }

    Expression visitCallExpression(ClassReferenceExp exp)
    {
        auto expressionExp = getFieldValue(exp, Id.expression);
        auto expression = fromMacroAst(expressionExp);

        auto argumentsExp = getFieldValue(exp, Id.arguments);
        auto arrayLiteral = fromMacroAst(argumentsExp);
        auto arguments = expToArray!(Expression, fromMacroAst)(arrayLiteral);

        auto declExp = getFieldValue(exp, Id.functionDeclaration);
        auto decl = expToFuncDeclaration(fromMacroAst(declExp));

        auto callExp = new CallExp(loc, expression, arguments);
        callExp.f = decl;

        return callExp;
    }

    Expression visitIntegerExp(ClassReferenceExp exp)
    {
        auto value = getFieldValue(exp, Id.value);
        assert(isValidIntegerExp(value));

        return value;
    }

    Expression visitStringExpression(ClassReferenceExp exp)
    {
        auto value = getFieldValue(exp, Id.value);
        assert(value.op == TOKstring);

        return value;
    }

    Expression visitFunctionDeclaration(ClassReferenceExp exp)
    {
        auto identExp = getFieldValue(exp, Id.ident);
        auto ident = expToIdent(identExp);

        auto typeExp = getFieldValue(exp, Id.type);
        auto type = expToType(fromMacroAst(typeExp));
        assert(type.ty == Tfunction);

        auto bodyExp = getFieldValue(exp, Id.body_);
        auto a = fromMacroAst(bodyExp);
        auto body_ = expToStatement(a);

        auto func = new FuncDeclaration(loc, loc, ident, STCundefined, type);
        func.linkage = (cast(TypeFunction) type).linkage;

        return new DeclarationExp(loc, func);
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

        // var.parent = md;
        // var.setScope(md._scope);
        invocation.addMember(var);


        return new AstMacroResultInvocationExp(loc, invocation);
        // auto a = new DeclarationExp(loc, var);
        // return a;
    }

    Expression visitBasicType(ClassReferenceExp exp)
    {
        auto typeKind = getFieldValue(exp, Id.typeKind);
        auto ty = expToTY(typeKind);
        Type type;

        switch(ty)
        {
            case Tint32: type = Type.tint32; break;
            case Tvoid: type = Type.tvoid; break;
            default:
                printf("visitTypeBasic ty=%d\n", ty);
                assert(0);
        }

        return new TypeExp(loc, type);
    }

    Expression visitFunctionType(ClassReferenceExp exp)
    {
        auto paramsExp = getFieldValue(exp, Id.parameters);
        auto params = expToParameters(paramsExp);

        auto returnTypeExp = getFieldValue(exp, Id.next);
        auto returnType = expToType(fromMacroAst(returnTypeExp));

        auto variadicTypeExp = getFieldValue(exp, Id.variadicType);
        auto variadicType = 0;//cast(int) expToUlong(fromMacroAst(variadicTypeExp));

        auto linkageExp = getFieldValue(exp, Id.linkage);
        auto linkage = expToLINK(linkageExp);

        auto type = new TypeFunction(params, returnType, variadicType, linkage);

        return new TypeExp(loc, type);
    }

    VarExp visitVariableExpression(ClassReferenceExp exp)
    {
        auto varExp = getFieldValue(exp, Id.variable);
        auto var = expToDeclaration(fromMacroAst(varExp));

        return new VarExp(loc, var);
    }

    Expression visitEnumType(ClassReferenceExp exp)
    {
        auto decl = getFieldValue(exp, Id.declaration);
        decl = fromMacroAst(decl);
        println(decl.op);
        assert(0);
    }

    Expression visitEnumDeclaration(ClassReferenceExp exp)
    {
        auto type = getFieldValue(exp, Id.type);
        // type = fromMacroAst(type);

        auto memberType = getFieldValue(exp, Id.memberType);
        memberType = fromMacroAst(memberType);

        assert(0);
    }

    Expression visitCompoundStatement(ClassReferenceExp exp)
    {
        auto statementsExp = getFieldValue(exp, Id.statements);
        auto arrayLiteral = fromMacroAst(statementsExp);
        auto statements = expToArray!(Statement, expToStatement)(arrayLiteral);

        auto s = new CompoundStatement(loc, statements);
        return new AstMacroResultStatementExp(loc, s);
    }

    Expression visitExpressionStatement(ClassReferenceExp exp)
    {
        auto e = getFieldValue(exp, Id.expression);
        e = fromMacroAst(e);

        auto s = new ExpStatement(loc, e);
        return new AstMacroResultStatementExp(loc, s);
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
            printf("Could not find the node type '%s' in the dispatch table\n", to!string(nodeType).toStringz);
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

    ClassReferenceExp getFieldAsClassExp(ClassReferenceExp exp, Identifier ident)
    {
        auto e = getFieldValue(exp, ident);
        assert(e.op == TOKclassreference);

        return cast(ClassReferenceExp) e;
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
            default:
                println(exp.op);
                return false;
        }
    }

    Identifier expToIdent(Expression exp)
    {
        assert(exp.op == TOKstring);
        auto stringExp = cast(StringExp) exp;
        auto name = stringExp.peekSlice();

        return Identifier.idPool(name);
        // return invocation.generateId(name);
    }

    ulong expToUlong(Expression exp)
    {
        assert(isValidIntegerExp(exp));
        auto intExp = cast(IntegerExp) exp;
        return intExp.toInteger();
    }

    TY expToTY(Expression exp)
    {
        auto value = expToUlong(exp);
        assert(value >= 0 && value < TMAX);

        return cast(TY) value;
    }

    LINK expToLINK(Expression exp)
    {
        auto value = expToUlong(exp);
        assert(LINK.min >= 0 && value < LINK.max);

        return cast(LINK) value;
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

    Statement expToStatement(Expression exp)
    {
        if (exp is null)
            return null;

        assert(exp.op == TOKast_macro_result_exp);
        auto res = cast(AstMacroResultExp) exp;

        auto s = res.isStatement;
        assert(s);

        return s.statement;
    }

    Parameters* expToParameters(Expression exp)
    {
        assert(exp.op == TOKnull);
        return null;
    }

    Declaration expToDeclaration(Expression exp)
    {
        assert(exp.op == TOKdeclaration);
        auto declExp = cast(DeclarationExp) exp;

        auto decl = declExp.declaration.isDeclaration;
        assert(decl);

        return decl;
    }

    FuncDeclaration expToFuncDeclaration(Expression exp)
    {
        auto decl = expToDeclaration(exp);

        auto func = decl.isFuncDeclaration;
        assert(func);

        return func;
    }

    Array!(T)* expToArray(T, alias func)(Expression exp)
    {
        assert(exp.op == TOKarrayliteral);
        auto arrayLiterlExp = cast(ArrayLiteralExp) exp;

        auto array = new Array!(T);
        array.reserve(arrayLiterlExp.elements.dim);

        foreach (e ; *arrayLiterlExp.elements)
            array.push(func(e));

        return array;
    }
}

