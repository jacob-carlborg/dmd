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

extern (C) int printf(in char*, ...);

void print()
{
    printf("printing\n");
}

macro foo(AddExp a)
{
    return ast({
        print();
    });

    // return new ExpressionStatement(new CallExpression(new VariableExpression(new FunctionDeclaration("print", new FunctionType([], new BasicType(TypeKind.void_), VariadicType.nonVariadic, Linkage.d), new CompoundStatement([new ExpressionStatement(new CallExpression(new VariableExpression(new FunctionDeclaration("printf", new FunctionType([new Parameter(0LU, new PointerType(new BasicType(TypeKind.char_)), null, null)], new BasicType(TypeKind.int32), VariadicType.untyped, Linkage.c), null)), new FunctionDeclaration("printf", new FunctionType([new Parameter(0LU, new PointerType(new BasicType(TypeKind.char_)), null, null)], new BasicType(TypeKind.int32), VariadicType.untyped, Linkage.c), null), [new StringExpression("printing\x0a")]))]))), new FunctionDeclaration("print", new FunctionType([], new BasicType(TypeKind.void_), VariadicType.nonVariadic, Linkage.d), new CompoundStatement([new ExpressionStatement(new CallExpression(new VariableExpression(new FunctionDeclaration("printf", new FunctionType([new Parameter(0LU, new PointerType(new BasicType(TypeKind.char_)), null, null)], new BasicType(TypeKind.int32), VariadicType.untyped, Linkage.c), null)), new FunctionDeclaration("printf", new FunctionType([new Parameter(0LU, new PointerType(new BasicType(TypeKind.char_)), null, null)], new BasicType(TypeKind.int32), VariadicType.untyped, Linkage.c), null), [new StringExpression("printing\x0a")]))])), []));
}

// macro foo(AstNode n)
// {
//     return n;
// }


void main()
{
    foo(4 + 5);
    // int bar = 3;
    // auto a = foo(4 + 5);
    // println(a);
    // toDecl(4 + 5);
    // println(bar);
    // println("ok");
}

