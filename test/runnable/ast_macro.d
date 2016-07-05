// helpers
template ReturnType(alias T)
{
    static if (is(typeof(T) R == return))
        alias ReturnType = R;
    else
        static assert("no return type");
}

alias AliasSeq(T...) = T;

private macro privateAttribute(AstNode node) { return node; }

// tests
void testReturnTypes()
{
    macro inferredReturnType(AstNode node) { return node; }
    assert(is(ReturnType!inferredReturnType == AstNode));

    macro AstNode explicitReturnType(AstNode node) { return node; }
    assert(is(ReturnType!(explicitReturnType) == AstNode));

    assert(is(ReturnType!privateAttribute == AstNode));

    static macro otherAttribute(AstNode node) { return node; }
    assert(is(ReturnType!(otherAttribute) == AstNode));

    pure macro AstNode attributeExplicitReturnType(AstNode node) { return node; }
    assert(is(ReturnType!attributeExplicitReturnType == AstNode));

    enum uda;
    @uda macro withUda(AstNode node) { return node; }
    assert(is(ReturnType!(withUda) == AstNode));

    macro templateParameters(T)(T node) { return node; }
    assert(is(ReturnType!(templateParameters!(AstNode)) == AstNode));
}

void testAttributes()
{
    assert(__traits(getProtection, privateAttribute) == "private");

    static macro staticAttribute(AstNode node) { return node; }
    assert(__traits(isStaticFunction, staticAttribute));

    enum uda;
    @uda macro getUda(AstNode node) { return node; }
    assert(is(AliasSeq!(__traits(getAttributes, getUda))[0] == uda));
}

void testAstNodes()
{
    macro integerExp(IntegerExp e) { return e; }
    assert(is(ReturnType!(integerExp) == IntegerExp));
    assert(integerExp(1) == 1);

    macro addExp(AddExp e) { return e; }
    assert(is(ReturnType!(addExp) == AddExp));
    assert(addExp(1 + 1) == 2);

    macro varDeclaration()
    {
        auto type = new TypeBasic(TypeKind.int32);
        auto id = "bar";
        return VarDeclaration(id, type);
    }
    assert(is(ReturnType!(varDeclaration) == VarDeclaration));
    varDeclaration();
    assert(!__traits(compiles, { auto a = bar; }));
}

void testExtractExpression()
{
    macro extract(AddExp e)
    {
        return e.left;
    }

    assert(extract(2 + 1) == 2);
}

void main()
{
    testReturnTypes();
    testAttributes();
    testAstNodes();
    testExtractExpression();
}
