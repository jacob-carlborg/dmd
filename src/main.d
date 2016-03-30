module main;

import std.stdio : println = writeln;

import core.ast;

// macro foo (AddExp a)
// {
//     // if (__ctfe)
//     //     return "ctfe";
//     // else
//     //     return "not ctfe";
//     auto i = cast(IntegerExp) a.left;
//     assert(i);
//     return new IntegerExp(i.value);
// }

macro toDecl(Expression exp)
{
    auto type = new BasicType(TypeKind.int32);
    auto id = "bar";
    auto decl = VarDeclaration(id, type);

    return decl;
}

void main()
{
    int bar = 3;
    // auto a = foo(4 + 5);
    // println(a);
    toDecl(4 + 5);
    println(bar);
    println("ok");
}
