/*
TEST_OUTPUT:
---
fail_compilation/ast_macro.d(15): Error: macro ast_macro.astMacro needs to return a value of type core.ast.ast_node.AstNode, not (0) of type int
---
*/

macro astMacro()
{
    return 0;
}

void test()
{
    astMacro();
}

