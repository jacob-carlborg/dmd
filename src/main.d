// module main;

import std.stdio : println = writeln;

import core.ast;

macro foo(AstNode node)
{
    return node;
}

void main()
{
    foo({ int a; });
    println("ok");
}
