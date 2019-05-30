/**
 * Compiler implementation of the
 * $(LINK2 http://www.dlang.org, D programming language).
 *
 * Copyright:   Copyright (C) 1999-2019 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 http://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/ast_node.d, _ast_node.d)
 * Documentation:  https://dlang.org/phobos/dmd_ast_node.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/ast_node.d
 */
module dmd.ast_node;

import dmd.root.rootobject : RootObject;
import dmd.visitor : Visitor;

/// The base class of all AST nodes.
extern (C++) abstract class ASTNode : RootObject
{
    /// AST node kinds
    enum Kind
    {
        addAssignExp,
        addExp,
        addrExp,
        aliasDeclaration,
        aliasThis,
        alignDeclaration,
        andAssignExp,
        andExp,
        anonDeclaration,
        arrayExp,
        arrayInitializer,
        arrayLengthExp,
        arrayLiteralExp,
        arrayScopeSymbol,
        asmStatement,
        assertExp,
        assignExp,
        assocArrayLiteralExp,
        binAssignExp,
        blitExp,
        breakStatement,
        callExp,
        caseRangeStatement,
        caseStatement,
        castExp,
        catAssignExp,
        catch_,
        catDcharAssignExp,
        catElemAssignExp,
        catExp,
        classDeclaration,
        classReferenceExp,
        cmpExp,
        comExp,
        commaExp,
        compileDeclaration,
        compileExp,
        compileStatement,
        complexExp,
        compoundAsmStatement,
        compoundDeclarationStatement,
        compoundStatement,
        condExp,
        conditionalDeclaration,
        conditionalStatement,
        constructExp,
        continueStatement,
        cppMangleDeclaration,
        ctfeExp,
        ctorDeclaration,
        debugCondition,
        debugStatement,
        debugSymbol,
        declarationExp,
        defaultInitExp,
        defaultStatement,
        delegateExp,
        delegateFuncptrExp,
        delegatePtrExp,
        deleteDeclaration,
        deleteExp,
        deprecatedDeclaration,
        divAssignExp,
        divExp,
        dollarExp,
        doStatement,
        dotExp,
        dotIdExp,
        dotTemplateExp,
        dotTemplateInstanceExp,
        dotTypeExp,
        dotVarExp,
        dsymbol,
        dsymbolExp,
        dtorDeclaration,
        dtorExpStatement,
        dvCondition,
        enumDeclaration,
        enumMember,
        equalExp,
        errorExp,
        errorInitializer,
        errorStatement,
        expInitializer,
        expressionDsymbol,
        expStatement,
        fileInitExp,
        foreachRangeStatement,
        foreachStatement,
        forStatement,
        forwardingAttribDeclaration,
        forwardingScopeDsymbol,
        forwardingStatement,
        funcAliasDeclaration,
        funcDeclaration,
        funcExp,
        funcInitExp,
        funcLiteralDeclaration,
        gccAsmStatement,
        gotoCaseStatement,
        gotoDefaultStatement,
        gotoStatement,
        haltExp,
        identifierExp,
        identityExp,
        ifStatement,
        Import,
        importExp,
        importStatement,
        indexExp,
        inExp,
        Initializer,
        inlineAsmStatement,
        integerExp,
        interfaceDeclaration,
        intervalExp,
        invariantDeclaration,
        isExp,
        labelDsymbol,
        labelStatement,
        lineInitExp,
        linkDeclaration,
        logicalExp,
        minAssignExp,
        minExp,
        modAssignExp,
        modExp,
        module_,
        moduleInitExp,
        mulAssignExp,
        mulExp,
        negExp,
        newAnonClassExp,
        newDeclaration,
        newExp,
        notExp,
        nspace,
        nullExp,
        objcClassReferenceExp,
        orAssignExp,
        orExp,
        overDeclaration,
        overExp,
        overloadSet,
        Package,
        Parameter,
        peelStatement,
        postBlitDeclaration,
        postExp,
        powAssignExp,
        powExp,
        pragmaDeclaration,
        pragmaStatement,
        preExp,
        prettyFuncInitExp,
        protDeclaration,
        ptrExp,
        realExp,
        removeExp,
        returnStatement,
        scopeDsymbol,
        scopeExp,
        scopeGuardStatement,
        scopeStatement,
        sharedStaticCtorDeclaration,
        sharedStaticDtorDeclaration,
        shlAssignExp,
        shlExp,
        shrAssignExp,
        shrExp,
        sliceExp,
        staticAssert,
        staticAssertStatement,
        staticCtorDeclaration,
        staticDtorDeclaration,
        staticForeachDeclaration,
        staticForeachStatement,
        staticIfCondition,
        staticIfDeclaration,
        storageClassDeclaration,
        stringExp,
        structDeclaration,
        structInitializer,
        structLiteralExp,
        superExp,
        switchErrorStatement,
        switchStatement,
        symbolDeclaration,
        symbolExp,
        symOffExp,
        synchronizedStatement,
        templateAliasParameter,
        templateDeclaration,
        templateExp,
        templateInstance,
        templateMixin,
        templateParameter,
        templateThisParameter,
        templateTupleParameter,
        templateTypeParameter,
        templateValueParameter,
        thisDeclaration,
        thisExp,
        thrownExceptionExp,
        throwStatement,
        traitsExp,
        tryCatchStatement,
        tryFinallyStatement,
        tupleDeclaration,
        tupleExp,
        typeAArray,
        typeBasic,
        typeClass,
        typeDArray,
        typeDeduced,
        typeDelegate,
        typeEnum,
        typeError,
        typeExp,
        typeFunction,
        typeIdentifier,
        typeidExp,
        typeInfoArrayDeclaration,
        typeInfoAssociativeArrayDeclaration,
        typeInfoClassDeclaration,
        typeInfoConstDeclaration,
        typeInfoDeclaration,
        typeInfoDelegateDeclaration,
        typeInfoEnumDeclaration,
        typeInfoFunctionDeclaration,
        typeInfoInterfaceDeclaration,
        typeInfoInvariantDeclaration,
        typeInfoPointerDeclaration,
        typeInfoSharedDeclaration,
        typeInfoStaticArrayDeclaration,
        typeInfoStructDeclaration,
        typeInfoTupleDeclaration,
        typeInfoVectorDeclaration,
        typeInfoWildDeclaration,
        typeInstance,
        typeNull,
        typePointer,
        typeReference,
        typeReturn,
        typeSArray,
        typeSlice,
        typeStruct,
        typeTraits,
        typeTuple,
        typeTypeof,
        typeVector,
        uAddExp,
        unionDeclaration,
        unitTestDeclaration,
        unrolledLoopStatement,
        userAttributeDeclaration,
        ushrAssignExp,
        ushrExp,
        varDeclaration,
        varExp,
        vectorArrayExp,
        vectorExp,
        versionCondition,
        versionSymbol,
        voidInitExp,
        voidInitializer,
        whileStatement,
        withScopeSymbol,
        withStatement,
        xorAssignExp,
        xorExp
    }

    // AddAssignExp
    // AddExp
    // AddrExp
    // AggregateDeclaration
    // AliasDeclaration
    // AliasThis
    // AlignDeclaration
    // AndAssignExp
    // AndExp
    // AnonDeclaration
    // ArrayExp
    // ArrayInitializer
    // ArrayLengthExp
    // ArrayLiteralExp
    // ArrayScopeSymbol
    // AsmStatement
    // AssertExp
    // AssignExp
    // AssocArrayLiteralExp
    // AttribDeclaration
    // BinAssignExp
    // BinExp
    // BlitExp
    // BreakStatement
    // CallExp
    // CaseRangeStatement
    // CaseStatement
    // CastExp
    // CatAssignExp
    // CatExp
    // ClassDeclaration
    // ClassReferenceExp
    // CmpExp
    // ComExp
    // CommaExp
    // CompileDeclaration
    // CompileExp
    // CompileStatement
    // ComplexExp
    // CompoundAsmStatement
    // CompoundDeclarationStatement
    // CompoundStatement
    // CondExp
    // Condition
    // ConditionalDeclaration
    // ConditionalStatement
    // ConstructExp
    // ContinueStatement
    // CPPMangleDeclaration
    // CtorDeclaration
    // DebugCondition
    // DebugStatement
    // DebugSymbol
    // Declaration
    // DeclarationExp
    // DefaultInitExp
    // DefaultStatement
    // DelegateExp
    // DelegateFuncptrExp
    // DelegatePtrExp
    // DeleteDeclaration
    // DeleteExp
    // DeprecatedDeclaration
    // DivAssignExp
    // DivExp
    // DollarExp
    // DoStatement
    // DotExp
    // DotIdExp
    // DotTemplateExp
    // DotTemplateInstanceExp
    // DotTypeExp
    // DotVarExp
    // Dsymbol
    // DsymbolExp
    // DtorDeclaration
    // DtorExpStatement
    // DVCondition
    // EnumDeclaration
    // EnumMember
    // EqualExp
    // ErrorExp
    // ErrorInitializer
    // ErrorStatement
    // ExpInitializer
    // Expression
    // ExpStatement
    // FileInitExp
    // ForeachRangeStatement
    // ForeachStatement
    // ForStatement
    // ForwardingStatement
    // FuncAliasDeclaration
    // FuncDeclaration
    // FuncExp
    // FuncInitExp
    // FuncLiteralDeclaration
    // GccAsmStatement
    // GotoCaseStatement
    // GotoDefaultStatement
    // GotoStatement
    // HaltExp
    // IdentifierExp
    // IdentityExp
    // IfStatement
    // Import
    // ImportExp
    // ImportStatement
    // IndexExp
    // InExp
    // Initializer
    // InlineAsmStatement
    // IntegerExp
    // InterfaceDeclaration
    // IntervalExp
    // InvariantDeclaration
    // IsExp
    // LabelDsymbol
    // LabelStatement
    // LineInitExp
    // LinkDeclaration
    // LogicalExp
    // MinAssignExp
    // MinExp
    // ModAssignExp
    // ModExp
    // Module
    // ModuleInitExp
    // MulAssignExp
    // MulExp
    // NegExp
    // NewAnonClassExp
    // NewDeclaration
    // NewExp
    // NotExp
    // Nspace
    // NullExp
    // ObjcClassReferenceExp
    // OrAssignExp
    // OrExp
    // OverDeclaration
    // OverExp
    // OverloadSet
    // Package
    // Parameter
    // PeelStatement
    // PostBlitDeclaration
    // PostExp
    // PowAssignExp
    // PowExp
    // PragmaDeclaration
    // PragmaStatement
    // PreExp
    // PrettyFuncInitExp
    // ProtDeclaration
    // PtrExp
    // RealExp
    // RemoveExp
    // ReturnStatement
    // ScopeDsymbol
    // ScopeExp
    // ScopeGuardStatement
    // ScopeStatement
    // SharedStaticCtorDeclaration
    // SharedStaticDtorDeclaration
    // ShlAssignExp
    // ShlExp
    // ShrAssignExp
    // ShrExp
    // SliceExp
    // Statement
    // StaticAssert
    // StaticAssertStatement
    // StaticCtorDeclaration
    // StaticDtorDeclaration
    // StaticForeachDeclaration
    // StaticForeachStatement
    // StaticIfCondition
    // StaticIfDeclaration
    // StorageClassDeclaration
    // StringExp
    // StructDeclaration
    // StructInitializer
    // StructLiteralExp
    // SuperExp
    // SwitchErrorStatement
    // SwitchStatement
    // SymbolDeclaration
    // SymbolExp
    // SymOffExp
    // SynchronizedStatement
    // TemplateAliasParameter
    // TemplateDeclaration
    // TemplateExp
    // TemplateInstance
    // TemplateMixin
    // TemplateParameter
    // TemplateThisParameter
    // TemplateTupleParameter
    // TemplateTypeParameter
    // TemplateValueParameter
    // ThisDeclaration
    // ThisExp
    // ThrownExceptionExp
    // ThrowStatement
    // TraitsExp
    // TryCatchStatement
    // TryFinallyStatement
    // TupleDeclaration
    // TupleExp
    // Type
    // TypeAArray
    // TypeArray
    // TypeBasic
    // TypeClass
    // TypeDArray
    // TypeDelegate
    // TypeEnum
    // TypeError
    // TypeExp
    // TypeFunction
    // TypeIdentifier
    // TypeidExp
    // TypeInfoArrayDeclaration
    // TypeInfoAssociativeArrayDeclaration
    // TypeInfoClassDeclaration
    // TypeInfoConstDeclaration
    // TypeInfoDeclaration
    // TypeInfoDelegateDeclaration
    // TypeInfoEnumDeclaration
    // TypeInfoFunctionDeclaration
    // TypeInfoInterfaceDeclaration
    // TypeInfoInvariantDeclaration
    // TypeInfoPointerDeclaration
    // TypeInfoSharedDeclaration
    // TypeInfoStaticArrayDeclaration
    // TypeInfoStructDeclaration
    // TypeInfoTupleDeclaration
    // TypeInfoVectorDeclaration
    // TypeInfoWildDeclaration
    // TypeInstance
    // TypeNext
    // TypeNull
    // TypePointer
    // TypeQualified
    // TypeReference
    // TypeReturn
    // TypeSArray
    // TypeSlice
    // TypeStruct
    // TypeTraits
    // TypeTuple
    // TypeTypeof
    // TypeVector
    // UAddExp
    // UnaExp
    // UnionDeclaration
    // UnitTestDeclaration
    // UnrolledLoopStatement
    // UserAttributeDeclaration
    // UshrAssignExp
    // UshrExp
    // VarDeclaration
    // VarExp
    // VectorArrayExp
    // VectorExp
    // VersionCondition
    // VersionSymbol
    // VoidInitExp
    // VoidInitializer
    // WhileStatement
    // WithScopeSymbol
    // WithStatement
    // XorAssignExp
    // XorExp


    /// Identifies which kind of AST node this is.
    immutable Kind nodeKind;

    /**
     * Initializes this AST node.
     *
     * Params:
     *  nodeKind = the kind of this AST node
     */
    this(Kind nodeKind)
    {
        this.nodeKind = nodeKind;
    }

    /**
     * Visits this AST node using the given visitor.
     *
     * Params:
     *  v = the visitor to use when visiting this node
     */
    abstract void accept(Visitor v);
}
