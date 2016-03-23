module main;

import std.stdio : println = writeln;

import core.ast;

macro foo (AddExp a)
// macro foo (int a)
{
    // if (__ctfe)
    //     return "ctfe";
    // else
    //     return "not ctfe";
    auto i = cast(IntegerExp) a.left;
    assert(i);
    return new IntegerExp(i.value);
}

// enum abc = foo(1 + 3);

void main()
{
    auto a = foo(4 + 5);
    // auto b = cast(IntegerExp) a.left;
    // assert(b);
    // println(b.value);
    println(a);
    println("ok");
}


// void foo(int a)
// {
//
// }
//
//
// void main()
// {
//     foo("asd");
// }
