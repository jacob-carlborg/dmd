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

import ddmd.aggregate;
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
        auto result = toMacroAst(loc, arg, true);

        // printAst(result);
        // println(result.toChars.fromStringz);

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
                exp.type ? exp.type.toChars.fromStringz : "<unknown>", exp.op));
        }

        override void visit(AddExp exp)
        {
            auto left = toMacroAst(loc, exp.e1);
            auto right = toMacroAst(loc, exp.e2);
            auto type = toMacroAst(exp.type);

            auto ctorArgs = createCtorArgs(type, left, right);
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

            auto type = toMacroAst(exp.type);
            auto right = toMacroAst(loc, exp.e2);

            auto ctorArgs = createCtorArgs(type, left, right);

            result = createNewExp(macroAst.types.blitExpression, ctorArgs);

            if (foobar)
                foobar.arguments = ctorArgs;
        }

        override void visit(CallExp exp)
        {
            auto type = toMacroAst(exp.type);
            auto decl = toMacroAst(exp.f);
            auto e1 = toMacroAst(exp.e1);

            auto arrayType = macroAst.types.expression.arrayOf;
            auto arguments = toArrayLiteral(exp.arguments, arrayType);

            auto ctorArgs = createCtorArgs(type, e1, decl, arguments);

            result = createNewExp(macroAst.types.callExpression, ctorArgs);
        }

        override void visit(DeclarationExp exp)
        {
            auto type = toMacroAst(exp.type);
            auto decl = toMacroAst(loc, exp.declaration);
            auto ctorArgs = createCtorArgs(type, decl);

            result = createNewExp(macroAst.types.declarationExpression, ctorArgs);
        }

        override void visit(IntegerExp exp)
        {
            auto type = toMacroAst(exp.type);
            auto ctorArgs = createCtorArgs(type, exp);
            result = createNewExp(macroAst.types.integerExp, ctorArgs);
        }

        override void visit(FuncExp exp)
        {
            if (topLevel)
            {
                auto type = toMacroAst(exp.type);
                assert(exp.fd);
                auto decl = toMacroAst(exp.fd);assert(decl && decl.op != TOKnull);

                auto ctorArgs = createCtorArgs(type, decl);
                result = createNewExp(macroAst.types.functionExpression, ctorArgs);

                /*auto body_ = exp.fd.fbody.isCompoundStatement;
                assert(body_);

                auto arrayType = macroAst.types.statement.arrayOf;
                auto statements = toArrayLiteral(body_.statements, arrayType);
                auto ctorArgs = createCtorArgs(statements);

                result = createNewExp(macroAst.types.compoundStatement, ctorArgs);*/
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
            auto type = toMacroAst(exp.type);
            auto ctorArgs = createCtorArgs(type, exp);
            result = createNewExp(macroAst.types.stringExpression, ctorArgs);
        }

        override void visit(VarExp exp)
        {
            auto type = toMacroAst(exp.type);
            auto decl = toMacroAst(loc, exp.var);
            auto ctorArgs = createCtorArgs(type, decl);

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
            auto type = toMacroAst(fd.type);
            auto functionType = macroAst.nodeTypes[fd.tok];
            auto body_ = toMacroAst(fd.fbody);
            auto ident = toMacroAst(fd.ident);

            auto ctorArgs = createCtorArgs(type, functionType, body_, ident);
            result = createNewExp(macroAst.types.functionLiteralDeclaration, ctorArgs);
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
            auto deco = new StringExp(loc, t.deco);
            auto ctorArgs = createCtorArgs(exp, deco);

            result = createNewExp(macroAst.types.basicType, ctorArgs);
        }

        override void visit(TypeArray t)
        {
            auto ty = macroAst.typeKinds[t.ty];
            auto deco = new StringExp(loc, t.deco);
            auto next = toMacroAst(t.next);
            auto ctorArgs = createCtorArgs(ty, deco, next);

            result = createNewExp(macroAst.types.arrayType, ctorArgs);
        }

        override void visit(TypeEnum t)
        {
            auto decl = toMacroAst(t.sym);
            auto deco = new StringExp(loc, t.deco);
            auto ctorArgs = createCtorArgs(deco, decl);

            result = createNewExp(macroAst.types.enumType, ctorArgs);
        }

        override void visit(TypeFunction t)
        {
            auto deco = new StringExp(loc, t.deco);
            auto arrayType = macroAst.types.parameter.arrayOf;
            auto parameters = toArrayLiteral(t.parameters, arrayType);

            auto returnType = toMacroAst(t.next);
            auto variadic = macroAst.variadicTypes[t.varargs];
            auto linkage = macroAst.linkages[t.linkage];

            auto ctorArgs = createCtorArgs(deco, parameters, returnType, variadic, linkage);
            result = createNewExp(macroAst.types.functionType, ctorArgs);
        }

        override void visit(TypePointer t)
        {
            auto deco = new StringExp(loc, t.deco);
            auto next = toMacroAst(t.next);
            auto ctorArgs = createCtorArgs(deco, next);

            result = createNewExp(macroAst.types.pointerType, ctorArgs);
        }

        override void visit(TypeTuple t)
        {
            auto deco = new StringExp(loc, t.deco);
            auto arrayType = macroAst.types.parameter.arrayOf;
            auto arguments = toArrayLiteral(t.arguments, arrayType);

            auto ctorArgs = createCtorArgs(deco, arguments);
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
            assert(exps.length > 0, "At least one argument have to be given");

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
            auto newExp = new NewExp(loc, null, null, type, args);
            newExp.type = type;

            return newExp;
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

        case TOKstring:
            return exp;

        case TOKint64:
            return exp;

        default:
            println("fromMacroAst ", exp.op);
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

    override const(char)* toChars()
    {
        return statement.toChars;
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
        Type functionLiteralDeclaration;
        Type variableDeclaration;

        Type expression;
        Type addExp;
        Type assignExpression;
        Type blitExpression;
        Type callExpression;
        Type declarationExpression;
        Type functionExpression;
        Type integerExp;
        Type stringExpression;
        Type symbolExpression;
        Type variableExpression;

        Type expressionInitializer;

        Type compoundStatement;
        Type expressionStatement;
        Type foreachStatement;
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
            functionLiteralDeclaration = findType(loc, modules.declaration, Id.FunctionLiteralDeclaration);

            expression = findType(loc, modules.expression, Id.Expression);
            addExp = findType(loc, modules.expression, Id.AddExp);
            assignExpression = findType(loc, modules.expression, Id.AssignExpression);
            blitExpression = findType(loc, modules.expression, Id.BlitExpression);
            callExpression = findType(loc, modules.expression, Id.CallExpression);
            declarationExpression = findType(loc, modules.expression, Id.DeclarationExpression);
            functionExpression = findType(loc, modules.expression, Id.FunctionExpression);
            integerExp = findType(loc, modules.expression, Id.IntegerExp);
            stringExpression = findType(loc, modules.expression, Id.StringExpression);
            symbolExpression = findType(loc, modules.expression, Id.SymbolExpression);
            variableExpression = findType(loc, modules.expression, Id.VariableExpression);

            expressionInitializer = findType(loc, modules.initializer, Id.ExpressionInitializer);

            compoundStatement = findType(loc, modules.statement, Id.CompoundStatement);
            expressionStatement = findType(loc, modules.statement, Id.ExpressionStatement);
            foreachStatement = findType(loc, modules.statement, Id.ForeachStatement);
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
    EnumProjection!TOK nodeTypes;
    EnumProjection!TY typeKinds;
    EnumProjection!int variadicTypes;
    EnumProjection!LINK linkages;
    bool initialized;

    void init(Loc loc)
    {
        modules.init();
        types.init(loc, modules);

        nodeTypes = EnumProjection!TOK(loc, modules.astNode, Id.NodeType, [
            TOKdeclaration: Id.delegateExpression,
            TOKfunction: Id.functionExpression,
            TOKreserved: Id.reserved
        ]);

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
        NodeType.functionExpression: &visitFunctionExpression,
        NodeType.integerExp: &visitIntegerExp,
        NodeType.stringExpression: &visitStringExpression,
        NodeType.functionDeclaration: &visitFunctionDeclaration,
        NodeType.functionLiteralDeclaration: &visitFunctionLiteralDeclaration,
        NodeType.varDeclaration: &visitVarDeclaration,
        NodeType.basicType: &visitBasicType,
        NodeType.functionType: &visitFunctionType,
        NodeType.variableExpression: &visitVariableExpression,
        NodeType.enumType: &visitEnumType,
        NodeType.pointerType: &visitPointerType,
        NodeType.enumDeclaration: &visitEnumDeclaration,
        NodeType.compoundStatement: &visitCompoundStatement,
        NodeType.expressionStatement: &visitExpressionStatement
    ];

    // This enum needs to match the one in the core.ast.ast_node
    private enum NodeType : short
    {
        reserved,

        astNode,

        declaration,
        enumDeclaration,
        varDeclaration,
        functionDeclaration,
        functionLiteralDeclaration,

        expression,
        arrayLiteralExpression,
        assignExpression,
        addExp,
        binExp,
        blitExpression,
        callExpression,
        declarationExpression,
        delegateExpression,
        functionExpression,
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
        foreachStatement,
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

        auto addExp = new AddExp(loc, e1, e2);
        addExp.type = extractType(exp);

        return addExp;
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
        callExp.type = extractType(exp);

        return callExp;
    }

    Expression visitFunctionExpression(ClassReferenceExp exp)
    {
        auto declarationExp = getFieldValue(exp, Id.declaration);
        auto declaration = expToDeclaration(fromMacroAst(declarationExp));

        auto returnType = Type.tvoid;
        auto functionType = new TypeFunction(new Parameters(), Type.tvoid, 0, LINKd);

        return new FuncExp(loc, declaration);
    }

    Expression visitIntegerExp(ClassReferenceExp exp)
    {
        auto value = getFieldValue(exp, Id.value);
        assert(isValidIntegerExp(value));
        value.type = extractType(exp);

        return value;
    }

    Expression visitStringExpression(ClassReferenceExp exp)
    {
        auto value = getFieldValue(exp, Id.value);
        assert(value.op == TOKstring);
        value.type = extractType(exp);

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
        func.fbody = body_;
        func.linkage = (cast(TypeFunction) type).linkage;
        func.parent = md.parent;

        return new DeclarationExp(loc, func);
    }

    Expression visitFunctionLiteralDeclaration(ClassReferenceExp exp)
    {
        auto func = expToFuncDeclaration(visitFunctionDeclaration(exp));
        auto funcLiteralDecl = new FuncLiteralDeclaration(loc, loc, func.type, TOKreserved, null, func.ident);
        funcLiteralDecl.fbody = func.fbody;
        funcLiteralDecl.linkage = func.linkage;
        funcLiteralDecl.parent = func.parent;

        return new DeclarationExp(loc, funcLiteralDecl);
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
            case Tchar: type = Type.tchar; break;
            default:
                println(cast(ENUMTY) ty);
                printf("visitTypeBasic ty=%d\n", ty);
                assert(0, "Unhandled basic type case: ");
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
        auto variadicType = cast(int) expToUlong(fromMacroAst(variadicTypeExp));

        auto linkageExp = getFieldValue(exp, Id.linkage);
        auto linkage = expToLINK(linkageExp);

        auto type = new TypeFunction(params, returnType, variadicType, linkage);
        type.deco = extractDeco(exp);

        return new TypeExp(loc, type);
    }

    VarExp visitVariableExpression(ClassReferenceExp exp)
    {
        auto varExp = getFieldValue(exp, Id.variable);
        auto var = expToDeclaration(fromMacroAst(varExp));

        auto v = new VarExp(loc, var);
        v.type = extractType(exp);

        return v;
    }

    Expression visitEnumType(ClassReferenceExp exp)
    {
        auto decl = getFieldValue(exp, Id.declaration);
        decl = fromMacroAst(decl);
        println(decl.op);
        assert(0);
    }

    Expression visitPointerType(ClassReferenceExp exp)
    {
        auto typeExp = getFieldValue(exp, Id.next);
        auto nextType = expToType(fromMacroAst(typeExp));
        auto type = new TypePointer(nextType);
        type.deco = extractDeco(exp);

        return new TypeExp(loc, type);
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
            printf("Could not find the node type '%s' in the dispatch table\n", enumValueToString(nodeType).toStringz);
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
                println("isValidIntegerExp ", exp.op);
                return false;
        }
    }

    Identifier expToIdent(Expression exp)
    {
        if (exp is null || exp.op == TOKnull)
            return null;

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
        auto type = exp.type;

        return type;
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

    Parameter expToParameter(Expression exp)
    {
        assert(exp.op == TOKclassreference);

        auto classExp = cast(ClassReferenceExp) exp;

        auto storageClassExp = getFieldValue(classExp, Id.storageClass);
        auto storageClass = expToUlong(fromMacroAst(storageClassExp));

        auto typeExp = getFieldValue(classExp, Id.type);
        auto type = expToType(fromMacroAst(typeExp));

        auto identExp = getFieldValue(classExp, Id.identifier);
        auto ident = expToIdent(fromMacroAst(identExp));

        auto defaultArg = getFieldValue(classExp, Id.defaultArgument);

        if (defaultArg.op == TOKnull)
            defaultArg = null;

        return new Parameter(storageClass, type, ident, defaultArg);
    }

    Parameters* expToParameters(Expression exp)
    {
        if (exp.op == TOKnull)
            return null;

        return expToArray!(Parameter, expToParameter)(exp);
    }

    Declaration expToDeclaration(Expression exp)
    {
        assert(exp);
        assert(exp.op == TOKdeclaration, "exp.op: " ~ enumValueToString(exp.op));
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
        auto array = new Array!(T);

        if (!exp)
            return array;

        assert(exp.op == TOKarrayliteral);
        auto arrayLiterlExp = cast(ArrayLiteralExp) exp;

        array.reserve(arrayLiterlExp.elements.dim);

        foreach (e ; *arrayLiterlExp.elements)
            array.push(func(e));

        return array;
    }

    Type extractType(ClassReferenceExp exp)
    {
        auto typeExp = getFieldValue(exp, Id.type);
        return expToType(fromMacroAst(typeExp));
    }

    char* extractDeco(Expression exp)
    {
        assert(exp.op == TOKclassreference, "exp.op: " ~ enumValueToString(exp.op));
        auto classExp = cast(ClassReferenceExp) exp;

        auto decoExp = getFieldValue(classExp, Id.deco);
        assert(decoExp.op == TOKstring);

        auto deco = cast(StringExp) decoExp;
        return deco.toPtr;
    }
}

template BaseType(A)
{
    static if (is(A P == super))
        alias BaseType = P[0];
    else
        static assert(0, "argument is not a class or interface");
}

static string enumValueToString(T)(T value) if (is(T == enum))
{
    foreach (name ; __traits(allMembers, T))
    {
        if (__traits(getMember, T, name) == value)
            return name;
    }

    return null;
}

public void printAst(T)(T node)
{
    import core.stdc.stdarg;
    import ddmd.root.outbuffer;

    static const(char)[] escape(const(char)[] str)
    {
        size_t j;

        static const(char)[] replace(const(char)[] s, size_t i, string replacement)
        {
            return s[0 .. i] ~ replacement ~ s[i + 1 .. $];
        }

        foreach (i, e ; str)
        {
            if (e == '\n')
                str = replace(str, i, "\\n");

            else if (e == '"')
                str = replace(str, i, "\\\"");
        }

        return str;
    }

    extern (C++) final scope class AstPrintVisitor : Visitor
    {
        OutBuffer* buffer;
        int level;
        enum indentation = 2;
        bool shouldIdent;

        alias visit = Visitor.visit;

        this(OutBuffer* buffer)
        {
            this.buffer = buffer;
        }

        override void visit(Identifier ident)
        {
            appendf("Identifier(%s)", ident.toChars);
        }

        override void visit(Statement s)
        {
            assert(0, "No implementation for the statement: " ~ astTypeName(s));
        }

        void visitBase(Statement)
        {
            node("Statement");
        }

        override void visit(CompoundStatement s)
        {
            node(s, {
                fields("statements", s.statements);
            });
        }

        void visitBase(CompoundStatement s)
        {
            visit(s);
        }

        override void visit(ExpStatement s)
        {
            node(s, {
                fields("exp", s.exp);
            });
        }

        override void visit(ForeachStatement s)
        {
            node(s, {
               fields(
                   "op", s.op,
                   "parameters", s.parameters,
                   "aggr", s.aggr,
                   "_body", s._body
                );
            });
        }

        override void visit(ReturnStatement s)
        {
            node(s, {
                fields("exp", s.exp);
            });
        }

        override void visit(Expression e)
        {
            assert(0, "No implementation for the expression: " ~ astTypeName(e));
        }

        void visitBase(Expression e)
        {
            node("Expression", {
                fields(
                    "type", e.type,
                    "op", e.op,
                );
            });
        }

        override void visit(ArrayLiteralExp e)
        {
            node(e, {
                fields("elements", e.elements);
            });
        }

        override void visit(AssignExp e)
        {
            node(e);
        }

        override void visit(AstMacroResultStatementExp e)
        {
            e.statement.accept(this);
        }

        override void visit(BinExp e)
        {
            node(e, {
                fields(
                    "e1", e.e1,
                    "e2", e.e2
                );
            });
        }

        void visitBase(BinExp e)
        {
            visit(e);
        }

        override void visit(CallExp e)
        {
            node(e, {
                fields(
                    "arguments", e.arguments,
                    "f", e.f
                );
            });
        }

        override void visit(ClassReferenceExp e)
        {
            node(e);
        }

        override void visit(FuncExp e)
        {
            node(e, {
                fields("fd", e.fd);
            });
        }

        override void visit(IdentifierExp e)
        {
            node(e, {
                fields("ident", e.ident);
            });
        }

        override void visit(IntegerExp e)
        {
            node(e, {
                fields("value", e.toInteger);
            });
        }

        override void visit(NewExp e)
        {
            node(e, {
                fields(
                    "thisexp", e.thisexp,
                    "newargs", e.newargs,
                    "newtype", e.newtype,
                    "arguments", e.arguments
                );
            });
        }

        override void visit(NullExp e)
        {
            node(e);
        }

        override void visit(StringExp e)
        {
            node(e, {
                fields("string", `"` ~ escape(e.peekSlice) ~ `"`);
            });
        }

        override void visit(SymbolExp e)
        {
            node(e, {
                fields("var", e.var);
            });
        }

        void visitBase(SymbolExp e)
        {
            visit(e);
        }

        override void visit(ThisExp e)
        {
            node(e, {
                fields("var", e.var);
            });
        }

        override void visit(UnaExp e)
        {
            node(e, {
                fields(
                    "expression", e.e1
                );
            });
        }

        void visitBase(UnaExp e)
        {
            visit(e);
        }

        override void visit(VarExp e)
        {
            node(e);
        }

        override void visit(Dsymbol s)
        {
            assert(0, "No implementation for the symbol: " ~ astTypeName(s));
        }

        void visitBase(Dsymbol s)
        {
            node("Dsymbol", {
                fields("ident", s.ident);
            });
        }

        override void visit(ScopeDsymbol s)
        {
            node(s, {
                // fields("members", s.members);
            });
        }

        void visitBase(ScopeDsymbol s)
        {
            visit(s);
        }

        override void visit(Declaration d)
        {
            node(d, {
                fields("type", d.type);
            });
        }

        void visitBase(Declaration d)
        {
            visit(d);
        }

        override void visit(AggregateDeclaration ad)
        {
            node(ad, {
                //fields("type", ad.type);
            });
        }

        void visitBase(AggregateDeclaration ad)
        {
            visit(ad);
        }

        override void visit(AttribDeclaration ad)
        {
            node(ad);
        }

        void visitBase(AttribDeclaration ad)
        {
            visit(ad);
        }

        override void visit(ClassDeclaration cd)
        {
            node(cd);
        }

        override void visit(EnumDeclaration ed)
        {
            node(ed, {
                // fields("memtype", ed.memtype);
            });
        }

        override void visit(FuncDeclaration fd)
        {
            node(fd, {
                fields(
                    "fbody", fd.fbody,
                    "fes", fd.fes,
                );
            });
        }

        void visitBase(FuncDeclaration fd)
        {
            visit(fd);
        }

        override void visit(FuncLiteralDeclaration fd)
        {
            node(fd, {
                fields("tok", fd.tok);
            });
        }

        override void visit(ProtDeclaration pd)
        {
            node(pd);
        }

        override void visit(Type t)
        {
            assert(0, "No implementation for the type: "  ~ astTypeName(t));
        }

        void visitBase(Type t)
        {
            // auto name = enumValueToString(cast(ENUMTY) t.ty);
            node("Type", {
                fields(
                    "ty", t.ty,
                    "deco", t.deco
                );
            });
            // appendf("Type(ty: %.*s)", name.length, name.ptr);
        }

        override void visit(TypeBasic t)
        {
            node(t, {
                fields("dstring", t.dstring);
            });
        }

        override void visit(TypeClass t)
        {
            node(t, {
                fields("sym", t.sym);
            });
        }

        override void visit(TypeEnum t)
        {
            node(t, {
                fields("sym", t.sym);
            });
        }

        override void visit(TypeFunction t)
        {
            node(t, {
                fields(
                    "parameters", t.parameters,
                    "varargs", t.varargs,
                    "linkage", t.linkage,
                );
            });
        }

        override void visit(TypeIdentifier t)
        {
            node(t, {
                fields("ident", t.ident);
            });
        }

        override void visit(TypeNext t)
        {
            node(t, {
                fields("next", t.next);
            });
        }

        void visitBase(TypeNext t)
        {
            visit(t);
        }

        override void visit(TypePointer t)
        {
            node(t);
        }

        override void visit(Parameter p)
        {
            node("Parameter", {
                fields(
                    "storageClass", p.storageClass,
                    "type", p.type,
                    "ident", p.ident,
                    "defaultArg", p.defaultArg
                );
            });
        }

        override void visit(TypeQualified t)
        {
            node(t);
        }

        void visitBase(TypeQualified t)
        {
            visit(t);
        }

        extern (D):

        void node(T)(T astNode, void delegate() block = null)
        {
            node(T.stringof, {
                visitBase(cast(BaseType!T) astNode);

                if (block)
                {
                    append(',');
                    newline();
                    block();
                }
            });
        }

        void node(string name, void delegate() block = null)
        {
            append(name);

            if (block)
                bracket(block);
            else
                append(" {}");
        }

        void fields(Fields...)(Fields fields) if (Fields.length % 2 == 0)
        {
            foreach (i, f ; fields)
            {
                static if (is(typeof(f) == string))
                {
                    if (i != 0)
                    {
                        append(',');
                        newline();
                    }

                    static if (is(Fields[i + 1] == enum))
                        fieldWithEnumValue(f, fields[i + 1]);
                    else
                        field(f, fields[i + 1]);
                }
                else
                    continue;
            }
        }

        void field(Node)(string name, Node node)
        {
            append(name);
            append(": ");

            if (node)
                node.accept(this);
            else
            {
                enum nodeName = Node.stringof;
                appendf("%.*s(null)", nodeName.length, nodeName.ptr);
            }
        }

        void field(T : int)(string name, T value)
        {
            append(name);
            append(": ");
            appendf("%d", value);
        }

        void field(T : ulong)(string name, T value)
        {
            append(name);
            append(": ");
            appendf("%d", value);
        }

        void field(T : const(char)*)(string name, T value)
        {
            append(name);
            append(": ");
            appendf("%s", value);
        }

        void field(T : const(char)[])(string name, T value)
        {
            append(name);
            append(": ");
            appendf("%.*s", value.length, value.ptr);
        }

        void field(T : string)(string name, T value)
        {
            field(name, cast(const(char)[]) value);
        }

        void fieldWithEnumValue(T)(string name, T value) if (is(T == enum))
        {
            field(name, enumValueToString(value));
        }

        void field(T : E*, E)(string name, T array)
        {
            append(name);
            append(":");

            if (!array)
                append(" null");

            else if (array.dim == 0)
                append(" []");

            else
            {
                bracket('[', ']', {
                    foreach (i, e ; *array)
                    {
                        if (i != 0)
                        {
                            append(',');
                            newline();
                        }

                        e.accept(this);
                    }
                });
            }
        }

        void bracket(void delegate() block)
        {
            bracket('{', '}', block);
        }

        void bracket(char s, char e, void delegate() block)
        {
            newline();
            append(s);
            newline();
            indent(block);
            newline();
            append(e);
        }

        void indent(void delegate() block)
        {
            level++;

            scope (exit)
                level--;

            block();
        }

        void indent()
        {
            if (shouldIdent)
            {
                foreach (_ ; 0 .. level * indentation)
                    buffer.writeByte(' ');
            }
        }

        void newline()
        {
            buffer.writenl();
            shouldIdent = true;
        }

        void append(string str)
        {
            indent();
            buffer.writestring(str);
            shouldIdent = false;
        }

        void append(char c)
        {
            indent();
            buffer.writeByte(c);
            shouldIdent = false;
        }

        extern (C++) void appendf(const(char)* format, ...) /*nothrow*/
        {
            indent();
            va_list ap;
            va_start(ap, format);
            buffer.vprintf(format, ap);
            va_end(ap);
            shouldIdent = false;
        }
    }

    OutBuffer buffer;

    scope visitor = new AstPrintVisitor(&buffer);
    node.accept(visitor);

    printf("%s\n", buffer.extractString);
}

import ddmd.attrib;
import ddmd.aliasthis;
import ddmd.aggregate;
import ddmd.complex;
import ddmd.cond;
import ddmd.ctfeexpr;
import ddmd.dclass;
import ddmd.declaration;
import ddmd.denum;
import ddmd.dimport;
import ddmd.declaration;
import ddmd.dstruct;
import ddmd.dsymbol;
import ddmd.dtemplate;
import ddmd.dversion;
import ddmd.expression;
import ddmd.func;
import ddmd.denum;
import ddmd.dimport;
import ddmd.dmodule;
import ddmd.mtype;
import ddmd.typinf;
import ddmd.identifier;
import ddmd.init;
import ddmd.globals;
import ddmd.doc;
import ddmd.root.rootobject;
import ddmd.statement;
import ddmd.staticassert;
import ddmd.nspace;
import ddmd.visitor;

string astTypeName(RootObject node)
{
    if (auto s = cast(Statement) node)
        return astTypeName(s);

    final switch(node.dyncast())
    {
        case DYNCAST_OBJECT:
            return "RootObject";
        case DYNCAST_EXPRESSION:
            return astTypeName(cast(Expression)node);
        case DYNCAST_DSYMBOL:
            return astTypeName(cast(Dsymbol)node);
        case DYNCAST_TYPE:
            return astTypeName(cast(Type)node);
        case DYNCAST_IDENTIFIER:
            return astTypeName(cast(Identifier)node);
        case DYNCAST_TUPLE:
            return astTypeName(cast(Tuple)node);
        case DYNCAST_PARAMETER:
            return astTypeName(cast(Parameter)node);
        // case DYNCAST_STATEMENT:
        //     return astTypeName(cast(Statement)node);
    }
}

string astTypeName(Dsymbol node)
{
    scope tsv = new AstTypeNameVisitor;
    node.accept(tsv);
    return tsv.typeName;
}

string astTypeName(Expression node)
{
    scope tsv = new AstTypeNameVisitor;
    node.accept(tsv);
    return tsv.typeName;
}

string astTypeName(Type node)
{
    scope tsv = new AstTypeNameVisitor;
    node.accept(tsv);
    return tsv.typeName;
}

string astTypeName(Identifier node)
{
    return "Identifier";
}

string astTypeName(Initializer node)
{
    scope tsv = new AstTypeNameVisitor;
    node.accept(tsv);
    return tsv.typeName;
}

string astTypeName(Condition node)
{
    scope tsv = new AstTypeNameVisitor;
    node.accept(tsv);
    return tsv.typeName;
}

string astTypeName(TemplateParameter node)
{
    scope tsv = new AstTypeNameVisitor;
    node.accept(tsv);
    return tsv.typeName;
}

string astTypeName(Statement node)
{
    scope tsv = new AstTypeNameVisitor;
    node.accept(tsv);
    return tsv.typeName;
}

extern(C++) final class AstTypeNameVisitor : Visitor
{
    alias visit = super.visit;
public :
    string typeName;


    void visit(RootObject node)
    {
        typeName = "RootObject";
    }

    override void visit(Condition node)
    {
        typeName = "Condition";
    }

    override void visit(Identifier node)
    {
        typeName = "Identifier";
    }

    override void visit(Statement node)
    {
        typeName = "Statement";
    }

    void visit(Catch node)
    {
        typeName = "Catch";
    }

    override void visit(Dsymbol node)
    {
        typeName = "Dsymbol";
    }

    void visit(DsymbolTable node)
    {
        typeName = "DsymbolTable";
    }

    void visit(Tuple node)
    {
        typeName = "Tuple";
    }

    override void visit(Initializer node)
    {
        typeName = "Initializer";
    }

    override void visit(Type node)
    {
        typeName = "Type";
    }

    override void visit(Parameter node)
    {
        typeName = "Parameter";
    }

    override void visit(Expression node)
    {
        typeName = "Expression";
    }

    override void visit(AliasThis node)
    {
        typeName = "AliasThis";
    }

    override void visit(DVCondition node)
    {
        typeName = "DVCondition";
    }

    override void visit(StaticIfCondition node)
    {
        typeName = "StaticIfCondition";
    }

    override void visit(ClassReferenceExp node)
    {
        typeName = "ClassReferenceExp";
    }

    override void visit(VoidInitExp node)
    {
        typeName = "VoidInitExp";
    }

    override void visit(ThrownExceptionExp node)
    {
        typeName = "ThrownExceptionExp";
    }

    void visit(CTFEExp node)
    {
        typeName = "CTFEExp";
    }

    override void visit(Import node)
    {
        typeName = "Import";
    }

    override void visit(DebugSymbol node)
    {
        typeName = "DebugSymbol";
    }

    override void visit(VersionSymbol node)
    {
        typeName = "VersionSymbol";
    }

    override void visit(StaticAssert node)
    {
        typeName = "StaticAssert";
    }

    override void visit(ErrorStatement node)
    {
        typeName = "ErrorStatement";
    }

    override void visit(PeelStatement node)
    {
        typeName = "PeelStatement";
    }

    override void visit(ExpStatement node)
    {
        typeName = "ExpStatement";
    }

    override void visit(CompileStatement node)
    {
        typeName = "CompileStatement";
    }

    override void visit(CompoundStatement node)
    {
        typeName = "CompoundStatement";
    }

    override void visit(UnrolledLoopStatement node)
    {
        typeName = "UnrolledLoopStatement";
    }

    override void visit(ScopeStatement node)
    {
        typeName = "ScopeStatement";
    }

    override void visit(WhileStatement node)
    {
        typeName = "WhileStatement";
    }

    override void visit(DoStatement node)
    {
        typeName = "DoStatement";
    }

    override void visit(ForStatement node)
    {
        typeName = "ForStatement";
    }

    override void visit(ForeachStatement node)
    {
        typeName = "ForeachStatement";
    }

    override void visit(ForeachRangeStatement node)
    {
        typeName = "ForeachRangeStatement";
    }

    override void visit(IfStatement node)
    {
        typeName = "IfStatement";
    }

    override void visit(ConditionalStatement node)
    {
        typeName = "ConditionalStatement";
    }

    override void visit(PragmaStatement node)
    {
        typeName = "PragmaStatement";
    }

    override void visit(StaticAssertStatement node)
    {
        typeName = "StaticAssertStatement";
    }

    override void visit(SwitchStatement node)
    {
        typeName = "SwitchStatement";
    }

    override void visit(CaseStatement node)
    {
        typeName = "CaseStatement";
    }

    override void visit(CaseRangeStatement node)
    {
        typeName = "CaseRangeStatement";
    }

    override void visit(DefaultStatement node)
    {
        typeName = "DefaultStatement";
    }

    override void visit(GotoDefaultStatement node)
    {
        typeName = "GotoDefaultStatement";
    }

    override void visit(GotoCaseStatement node)
    {
        typeName = "GotoCaseStatement";
    }

    override void visit(SwitchErrorStatement node)
    {
        typeName = "SwitchErrorStatement";
    }

    override void visit(ReturnStatement node)
    {
        typeName = "ReturnStatement";
    }

    override void visit(BreakStatement node)
    {
        typeName = "BreakStatement";
    }

    override void visit(ContinueStatement node)
    {
        typeName = "ContinueStatement";
    }

    override void visit(SynchronizedStatement node)
    {
        typeName = "SynchronizedStatement";
    }

    override void visit(WithStatement node)
    {
        typeName = "WithStatement";
    }

    override void visit(TryCatchStatement node)
    {
        typeName = "TryCatchStatement";
    }

    override void visit(TryFinallyStatement node)
    {
        typeName = "TryFinallyStatement";
    }

    override void visit(OnScopeStatement node)
    {
        typeName = "OnScopeStatement";
    }

    override void visit(ThrowStatement node)
    {
        typeName = "ThrowStatement";
    }

    override void visit(DebugStatement node)
    {
        typeName = "DebugStatement";
    }

    override void visit(GotoStatement node)
    {
        typeName = "GotoStatement";
    }

    override void visit(LabelStatement node)
    {
        typeName = "LabelStatement";
    }

    override void visit(LabelDsymbol node)
    {
        typeName = "LabelDsymbol";
    }

    override void visit(AsmStatement node)
    {
        typeName = "AsmStatement";
    }

    override void visit(ImportStatement node)
    {
        typeName = "ImportStatement";
    }

    override void visit(AttribDeclaration node)
    {
        typeName = "AttribDeclaration";
    }

    override void visit(Declaration node)
    {
        typeName = "Declaration";
    }

    override void visit(ScopeDsymbol node)
    {
        typeName = "ScopeDsymbol";
    }

    override void visit(OverloadSet node)
    {
        typeName = "OverloadSet";
    }

    void visit(TypeDeduced node)
    {
        typeName = "TypeDeduced";
    }

    override void visit(VoidInitializer node)
    {
        typeName = "VoidInitializer";
    }

    override void visit(ErrorInitializer node)
    {
        typeName = "ErrorInitializer";
    }

    override void visit(StructInitializer node)
    {
        typeName = "StructInitializer";
    }

    override void visit(ArrayInitializer node)
    {
        typeName = "ArrayInitializer";
    }

    override void visit(ExpInitializer node)
    {
        typeName = "ExpInitializer";
    }

    override void visit(TypeError node)
    {
        typeName = "TypeError";
    }

    override void visit(TypeNext node)
    {
        typeName = "TypeNext";
    }

    override void visit(TypeBasic node)
    {
        typeName = "TypeBasic";
    }

    override void visit(TypeVector node)
    {
        typeName = "TypeVector";
    }

    override void visit(TypeQualified node)
    {
        typeName = "TypeQualified";
    }

    override void visit(TypeStruct node)
    {
        typeName = "TypeStruct";
    }

    override void visit(TypeEnum node)
    {
        typeName = "TypeEnum";
    }

    override void visit(TypeClass node)
    {
        typeName = "TypeClass";
    }

    override void visit(TypeTuple node)
    {
        typeName = "TypeTuple";
    }

    override void visit(TypeNull node)
    {
        typeName = "TypeNull";
    }

    override void visit(IntegerExp node)
    {
        typeName = "IntegerExp";
    }

    override void visit(ErrorExp node)
    {
        typeName = "ErrorExp";
    }

    override void visit(RealExp node)
    {
        typeName = "RealExp";
    }

    override void visit(ComplexExp node)
    {
        typeName = "ComplexExp";
    }

    override void visit(IdentifierExp node)
    {
        typeName = "IdentifierExp";
    }

    override void visit(DsymbolExp node)
    {
        typeName = "DsymbolExp";
    }

    override void visit(ThisExp node)
    {
        typeName = "ThisExp";
    }

    override void visit(NullExp node)
    {
        typeName = "NullExp";
    }

    override void visit(StringExp node)
    {
        typeName = "StringExp";
    }

    override void visit(TupleExp node)
    {
        typeName = "TupleExp";
    }

    override void visit(ArrayLiteralExp node)
    {
        typeName = "ArrayLiteralExp";
    }

    override void visit(AssocArrayLiteralExp node)
    {
        typeName = "AssocArrayLiteralExp";
    }

    override void visit(StructLiteralExp node)
    {
        typeName = "StructLiteralExp";
    }

    override void visit(TypeExp node)
    {
        typeName = "TypeExp";
    }

    override void visit(ScopeExp node)
    {
        typeName = "ScopeExp";
    }

    override void visit(TemplateExp node)
    {
        typeName = "TemplateExp";
    }

    override void visit(NewExp node)
    {
        typeName = "NewExp";
    }

    override void visit(NewAnonClassExp node)
    {
        typeName = "NewAnonClassExp";
    }

    override void visit(SymbolExp node)
    {
        typeName = "SymbolExp";
    }

    override void visit(OverExp node)
    {
        typeName = "OverExp";
    }

    override void visit(FuncExp node)
    {
        typeName = "FuncExp";
    }

    override void visit(DeclarationExp node)
    {
        typeName = "DeclarationExp";
    }

    override void visit(TypeidExp node)
    {
        typeName = "TypeidExp";
    }

    override void visit(TraitsExp node)
    {
        typeName = "TraitsExp";
    }

    override void visit(HaltExp node)
    {
        typeName = "HaltExp";
    }

    override void visit(IsExp node)
    {
        typeName = "IsExp";
    }

    override void visit(UnaExp node)
    {
        typeName = "UnaExp";
    }

    override void visit(BinExp node)
    {
        typeName = "BinExp";
    }

    override void visit(IntervalExp node)
    {
        typeName = "IntervalExp";
    }

    override void visit(DefaultInitExp node)
    {
        typeName = "DefaultInitExp";
    }

    override void visit(DebugCondition node)
    {
        typeName = "DebugCondition";
    }

    override void visit(VersionCondition node)
    {
        typeName = "VersionCondition";
    }

    override void visit(EnumDeclaration node)
    {
        typeName = "EnumDeclaration";
    }

    override void visit(Package node)
    {
        typeName = "Package";
    }

    override void visit(Nspace node)
    {
        typeName = "Nspace";
    }

    override void visit(AggregateDeclaration node)
    {
        typeName = "AggregateDeclaration";
    }

    override void visit(DtorExpStatement node)
    {
        typeName = "DtorExpStatement";
    }

    override void visit(CompoundDeclarationStatement node)
    {
        typeName = "CompoundDeclarationStatement";
    }

    override void visit(CompoundAsmStatement node)
    {
        typeName = "CompoundAsmStatement";
    }

    override void visit(StorageClassDeclaration node)
    {
        typeName = "StorageClassDeclaration";
    }

    override void visit(LinkDeclaration node)
    {
        typeName = "LinkDeclaration";
    }

    override void visit(CPPMangleDeclaration node)
    {
        typeName = "CPPMangleDeclaration";
    }

    override void visit(ProtDeclaration node)
    {
        typeName = "ProtDeclaration";
    }

    override void visit(AlignDeclaration node)
    {
        typeName = "AlignDeclaration";
    }

    override void visit(AnonDeclaration node)
    {
        typeName = "AnonDeclaration";
    }

    override void visit(PragmaDeclaration node)
    {
        typeName = "PragmaDeclaration";
    }

    override void visit(ConditionalDeclaration node)
    {
        typeName = "ConditionalDeclaration";
    }

    override void visit(CompileDeclaration node)
    {
        typeName = "CompileDeclaration";
    }

    override void visit(UserAttributeDeclaration node)
    {
        typeName = "UserAttributeDeclaration";
    }

    override void visit(TupleDeclaration node)
    {
        typeName = "TupleDeclaration";
    }

    override void visit(AliasDeclaration node)
    {
        typeName = "AliasDeclaration";
    }

    override void visit(OverDeclaration node)
    {
        typeName = "OverDeclaration";
    }

    override void visit(VarDeclaration node)
    {
        typeName = "VarDeclaration";
    }

    override void visit(SymbolDeclaration node)
    {
        typeName = "SymbolDeclaration";
    }

    override void visit(WithScopeSymbol node)
    {
        typeName = "WithScopeSymbol";
    }

    override void visit(ArrayScopeSymbol node)
    {
        typeName = "ArrayScopeSymbol";
    }

    override void visit(TemplateDeclaration node)
    {
        typeName = "TemplateDeclaration";
    }

    override void visit(TemplateInstance node)
    {
        typeName = "TemplateInstance";
    }

    override void visit(FuncDeclaration node)
    {
        typeName = "FuncDeclaration";
    }

    override void visit(TypeArray node)
    {
        typeName = "TypeArray";
    }

    override void visit(TypePointer node)
    {
        typeName = "TypePointer";
    }

    override void visit(TypeReference node)
    {
        typeName = "TypeReference";
    }

    override void visit(TypeFunction node)
    {
        typeName = "TypeFunction";
    }

    override void visit(TypeDelegate node)
    {
        typeName = "TypeDelegate";
    }

    override void visit(TypeIdentifier node)
    {
        typeName = "TypeIdentifier";
    }

    override void visit(TypeInstance node)
    {
        typeName = "TypeInstance";
    }

    override void visit(TypeTypeof node)
    {
        typeName = "TypeTypeof";
    }

    override void visit(TypeReturn node)
    {
        typeName = "TypeReturn";
    }

    override void visit(TypeSlice node)
    {
        typeName = "TypeSlice";
    }

    override void visit(DollarExp node)
    {
        typeName = "DollarExp";
    }

    override void visit(SuperExp node)
    {
        typeName = "SuperExp";
    }

    override void visit(SymOffExp node)
    {
        typeName = "SymOffExp";
    }

    override void visit(VarExp node)
    {
        typeName = "VarExp";
    }

    override void visit(BinAssignExp node)
    {
        typeName = "BinAssignExp";
    }

    override void visit(CompileExp node)
    {
        typeName = "CompileExp";
    }

    override void visit(ImportExp node)
    {
        typeName = "ImportExp";
    }

    override void visit(AssertExp node)
    {
        typeName = "AssertExp";
    }

    override void visit(DotIdExp node)
    {
        typeName = "DotIdExp";
    }

    override void visit(DotTemplateExp node)
    {
        typeName = "DotTemplateExp";
    }

    override void visit(DotVarExp node)
    {
        typeName = "DotVarExp";
    }

    override void visit(DotTemplateInstanceExp node)
    {
        typeName = "DotTemplateInstanceExp";
    }

    override void visit(DelegateExp node)
    {
        typeName = "DelegateExp";
    }

    override void visit(DotTypeExp node)
    {
        typeName = "DotTypeExp";
    }

    override void visit(CallExp node)
    {
        typeName = "CallExp";
    }

    override void visit(AddrExp node)
    {
        typeName = "AddrExp";
    }

    override void visit(PtrExp node)
    {
        typeName = "PtrExp";
    }

    override void visit(NegExp node)
    {
        typeName = "NegExp";
    }

    override void visit(UAddExp node)
    {
        typeName = "UAddExp";
    }

    override void visit(ComExp node)
    {
        typeName = "ComExp";
    }

    override void visit(NotExp node)
    {
        typeName = "NotExp";
    }

    override void visit(DeleteExp node)
    {
        typeName = "DeleteExp";
    }

    override void visit(CastExp node)
    {
        typeName = "CastExp";
    }

    override void visit(VectorExp node)
    {
        typeName = "VectorExp";
    }

    override void visit(SliceExp node)
    {
        typeName = "SliceExp";
    }

    override void visit(ArrayLengthExp node)
    {
        typeName = "ArrayLengthExp";
    }

    override void visit(ArrayExp node)
    {
        typeName = "ArrayExp";
    }

    override void visit(DotExp node)
    {
        typeName = "DotExp";
    }

    override void visit(CommaExp node)
    {
        typeName = "CommaExp";
    }

    override void visit(DelegatePtrExp node)
    {
        typeName = "DelegatePtrExp";
    }

    override void visit(DelegateFuncptrExp node)
    {
        typeName = "DelegateFuncptrExp";
    }

    override void visit(IndexExp node)
    {
        typeName = "IndexExp";
    }

    override void visit(PostExp node)
    {
        typeName = "PostExp";
    }

    override void visit(PreExp node)
    {
        typeName = "PreExp";
    }

    override void visit(AssignExp node)
    {
        typeName = "AssignExp";
    }

    override void visit(AddExp node)
    {
        typeName = "AddExp";
    }

    override void visit(MinExp node)
    {
        typeName = "MinExp";
    }

    override void visit(CatExp node)
    {
        typeName = "CatExp";
    }

    override void visit(MulExp node)
    {
        typeName = "MulExp";
    }

    override void visit(DivExp node)
    {
        typeName = "DivExp";
    }

    override void visit(ModExp node)
    {
        typeName = "ModExp";
    }

    override void visit(PowExp node)
    {
        typeName = "PowExp";
    }

    override void visit(ShlExp node)
    {
        typeName = "ShlExp";
    }

    override void visit(ShrExp node)
    {
        typeName = "ShrExp";
    }

    override void visit(UshrExp node)
    {
        typeName = "UshrExp";
    }

    override void visit(AndExp node)
    {
        typeName = "AndExp";
    }

    override void visit(OrExp node)
    {
        typeName = "OrExp";
    }

    override void visit(XorExp node)
    {
        typeName = "XorExp";
    }

    override void visit(OrOrExp node)
    {
        typeName = "OrOrExp";
    }

    override void visit(AndAndExp node)
    {
        typeName = "AndAndExp";
    }

    override void visit(CmpExp node)
    {
        typeName = "CmpExp";
    }

    override void visit(InExp node)
    {
        typeName = "InExp";
    }

    override void visit(RemoveExp node)
    {
        typeName = "RemoveExp";
    }

    override void visit(EqualExp node)
    {
        typeName = "EqualExp";
    }

    override void visit(IdentityExp node)
    {
        typeName = "IdentityExp";
    }

    override void visit(CondExp node)
    {
        typeName = "CondExp";
    }

    override void visit(FileInitExp node)
    {
        typeName = "FileInitExp";
    }

    override void visit(LineInitExp node)
    {
        typeName = "LineInitExp";
    }

    override void visit(ModuleInitExp node)
    {
        typeName = "ModuleInitExp";
    }

    override void visit(FuncInitExp node)
    {
        typeName = "FuncInitExp";
    }

    override void visit(PrettyFuncInitExp node)
    {
        typeName = "PrettyFuncInitExp";
    }

    override void visit(ClassDeclaration node)
    {
        typeName = "ClassDeclaration";
    }

    override void visit(EnumMember node)
    {
        typeName = "EnumMember";
    }

    override void visit(Module node)
    {
        typeName = "Module";
    }

    override void visit(StructDeclaration node)
    {
        typeName = "StructDeclaration";
    }

    override void visit(DeprecatedDeclaration node)
    {
        typeName = "DeprecatedDeclaration";
    }

    override void visit(StaticIfDeclaration node)
    {
        typeName = "StaticIfDeclaration";
    }

    override void visit(TypeInfoDeclaration node)
    {
        typeName = "TypeInfoDeclaration";
    }

    override void visit(ThisDeclaration node)
    {
        typeName = "ThisDeclaration";
    }

    override void visit(TemplateMixin node)
    {
        typeName = "TemplateMixin";
    }

    override void visit(FuncAliasDeclaration node)
    {
        typeName = "FuncAliasDeclaration";
    }

    override void visit(FuncLiteralDeclaration node)
    {
        typeName = "FuncLiteralDeclaration";
    }

    override void visit(CtorDeclaration node)
    {
        typeName = "CtorDeclaration";
    }

    override void visit(PostBlitDeclaration node)
    {
        typeName = "PostBlitDeclaration";
    }

    override void visit(DtorDeclaration node)
    {
        typeName = "DtorDeclaration";
    }

    override void visit(StaticCtorDeclaration node)
    {
        typeName = "StaticCtorDeclaration";
    }

    override void visit(StaticDtorDeclaration node)
    {
        typeName = "StaticDtorDeclaration";
    }

    override void visit(InvariantDeclaration node)
    {
        typeName = "InvariantDeclaration";
    }

    override void visit(UnitTestDeclaration node)
    {
        typeName = "UnitTestDeclaration";
    }

    override void visit(NewDeclaration node)
    {
        typeName = "NewDeclaration";
    }

    override void visit(DeleteDeclaration node)
    {
        typeName = "DeleteDeclaration";
    }

    override void visit(TypeSArray node)
    {
        typeName = "TypeSArray";
    }

    override void visit(TypeDArray node)
    {
        typeName = "TypeDArray";
    }

    override void visit(TypeAArray node)
    {
        typeName = "TypeAArray";
    }

    override void visit(ConstructExp node)
    {
        typeName = "ConstructExp";
    }

    override void visit(BlitExp node)
    {
        typeName = "BlitExp";
    }

    override void visit(AddAssignExp node)
    {
        typeName = "AddAssignExp";
    }

    override void visit(MinAssignExp node)
    {
        typeName = "MinAssignExp";
    }

    override void visit(MulAssignExp node)
    {
        typeName = "MulAssignExp";
    }

    override void visit(DivAssignExp node)
    {
        typeName = "DivAssignExp";
    }

    override void visit(ModAssignExp node)
    {
        typeName = "ModAssignExp";
    }

    override void visit(AndAssignExp node)
    {
        typeName = "AndAssignExp";
    }

    override void visit(OrAssignExp node)
    {
        typeName = "OrAssignExp";
    }

    override void visit(XorAssignExp node)
    {
        typeName = "XorAssignExp";
    }

    override void visit(PowAssignExp node)
    {
        typeName = "PowAssignExp";
    }

    override void visit(ShlAssignExp node)
    {
        typeName = "ShlAssignExp";
    }

    override void visit(ShrAssignExp node)
    {
        typeName = "ShrAssignExp";
    }

    override void visit(UshrAssignExp node)
    {
        typeName = "UshrAssignExp";
    }

    override void visit(CatAssignExp node)
    {
        typeName = "CatAssignExp";
    }

    override void visit(InterfaceDeclaration node)
    {
        typeName = "InterfaceDeclaration";
    }

    override void visit(UnionDeclaration node)
    {
        typeName = "UnionDeclaration";
    }

    override void visit(TypeInfoStructDeclaration node)
    {
        typeName = "TypeInfoStructDeclaration";
    }

    override void visit(TypeInfoClassDeclaration node)
    {
        typeName = "TypeInfoClassDeclaration";
    }

    override void visit(TypeInfoInterfaceDeclaration node)
    {
        typeName = "TypeInfoInterfaceDeclaration";
    }

    override void visit(TypeInfoPointerDeclaration node)
    {
        typeName = "TypeInfoPointerDeclaration";
    }

    override void visit(TypeInfoArrayDeclaration node)
    {
        typeName = "TypeInfoArrayDeclaration";
    }

    override void visit(TypeInfoStaticArrayDeclaration node)
    {
        typeName = "TypeInfoStaticArrayDeclaration";
    }

    override void visit(TypeInfoAssociativeArrayDeclaration node)
    {
        typeName = "TypeInfoAssociativeArrayDeclaration";
    }

    override void visit(TypeInfoEnumDeclaration node)
    {
        typeName = "TypeInfoEnumDeclaration";
    }

    override void visit(TypeInfoFunctionDeclaration node)
    {
        typeName = "TypeInfoFunctionDeclaration";
    }

    override void visit(TypeInfoDelegateDeclaration node)
    {
        typeName = "TypeInfoDelegateDeclaration";
    }

    override void visit(TypeInfoTupleDeclaration node)
    {
        typeName = "TypeInfoTupleDeclaration";
    }

    override void visit(TypeInfoConstDeclaration node)
    {
        typeName = "TypeInfoConstDeclaration";
    }

    override void visit(TypeInfoInvariantDeclaration node)
    {
        typeName = "TypeInfoInvariantDeclaration";
    }

    override void visit(TypeInfoSharedDeclaration node)
    {
        typeName = "TypeInfoSharedDeclaration";
    }

    override void visit(TypeInfoWildDeclaration node)
    {
        typeName = "TypeInfoWildDeclaration";
    }

    override void visit(TypeInfoVectorDeclaration node)
    {
        typeName = "TypeInfoVectorDeclaration";
    }

    override void visit(SharedStaticCtorDeclaration node)
    {
        typeName = "SharedStaticCtorDeclaration";
    }

    override void visit(SharedStaticDtorDeclaration node)
    {
        typeName = "SharedStaticDtorDeclaration";
    }
}

