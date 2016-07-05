// parsing of AST macro declarations

enum uda;

macro inferredReturnType(AstNode node) { return node; }
macro AstNode explicitReturnType(AstNode node) { return node; }
private macro protectionAttribute(AstNode node) { return node; }
static macro otherAttribute(AstNode node) { return node; }
pure macro AstNode attributeExplicitReturnType(AstNode node) { return node; }
@uda macro withUda(AstNode node) { return node; }
macro templateParameters(T)(T node) { return node; }

void nested()
{
    macro inferredReturnType(AstNode node) { return node; }
    macro AstNode explicitReturnType(AstNode node) { return node; }
    static macro otherAttribute(AstNode node) { return node; }
    pure macro AstNode attributeExplicitReturnType(AstNode node) { return node; }
    @uda macro withUda(AstNode node) { return node; }
    macro templateParameters(T)(T node) { return node; }
}

