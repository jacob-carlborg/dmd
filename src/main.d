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
    // return ast({
    //     print();
    // });

    return new FunctionExpression
    (
        new PointerType
        (
            "PFZv",
            new FunctionType
            (
                "FZv",
                [],
                new BasicType
                (
                    TypeKind.void_, "v"),
                    VariadicType.nonVariadic, Linkage.d)
                ),
                new FunctionLiteralDeclaration
                (
                    new FunctionType
                    (
                        "FZv",
                        [],
                        new BasicType(TypeKind.void_, "v"),
                        VariadicType.nonVariadic,
                        Linkage.d
                    ),
                    NodeType.reserved,
                    new CompoundStatement
                    (
                        [
                            new ExpressionStatement
                            (
                                new CallExpression
                                (
                                    new BasicType(TypeKind.void_, "v"),
                                    new VariableExpression
                                    (
                                        new FunctionType
                                        (
                                            "FZv",
                                            [],
                                            new BasicType(TypeKind.void_, "v"),
                                            VariadicType.nonVariadic,
                                            Linkage.d
                                        ),
                                        new FunctionDeclaration
                                        (
                                            "print",
                                            new FunctionType
                                            (
                                                "FZv",
                                                [],
                                                new BasicType(TypeKind.void_, "v"),
                                                VariadicType.nonVariadic,
                                                Linkage.d
                                            ),
                                            new CompoundStatement
                                            (
                                                [
                                                    new ExpressionStatement
                                                    (
                                                        new CallExpression
                                                        (
                                                            new BasicType(TypeKind.int32, "i"),
                                                            new VariableExpression
                                                            (
                                                                new FunctionType
                                                                (
                                                                    "UxPaYi",
                                                                    [
                                                                        new Parameter
                                                                        (
                                                                            0LU,
                                                                            new PointerType
                                                                            (
                                                                                "xPa",
                                                                                new BasicType(TypeKind.char_, "xa")
                                                                            ),
                                                                            null,
                                                                            null
                                                                        )
                                                                    ],
                                                                    new BasicType(TypeKind.int32, "i"),
                                                                    VariadicType.untyped,
                                                                    Linkage.c
                                                                ),
                                                                new FunctionDeclaration
                                                                (
                                                                    "printf",
                                                                    new FunctionType
                                                                    (
                                                                        "UxPaYi",
                                                                        [
                                                                            new Parameter
                                                                            (
                                                                                0LU,
                                                                                new PointerType
                                                                                (
                                                                                    "xPa",
                                                                                    new BasicType(TypeKind.char_, "xa")
                                                                                ),
                                                                                null,
                                                                                null
                                                                            )
                                                                        ],
                                                                        new BasicType(TypeKind.int32, "i"),
                                                                        VariadicType.untyped,
                                                                        Linkage.c
                                                                    ),
                                                                    null
                                                                )
                                                            ),
                                                            new FunctionDeclaration
                                                            (
                                                                "printf",
                                                                new FunctionType
                                                                (
                                                                    "UxPaYi",
                                                                    [
                                                                        new Parameter
                                                                        (
                                                                            0LU,
                                                                            new PointerType
                                                                            (
                                                                                "xPa",
                                                                                new BasicType(TypeKind.char_, "xa")
                                                                            ),
                                                                            null,
                                                                            null
                                                                        )
                                                                    ],
                                                                    new BasicType(TypeKind.int32, "i"),
                                                                    VariadicType.untyped,
                                                                    Linkage.c
                                                                ),
                                                                null
                                                            ),
                                                            [
                                                                new StringExpression
                                                                (
                                                                    new PointerType
                                                                    (
                                                                        "xPa",
                                                                        new BasicType(TypeKind.char_, "xa")
                                                                    ),
                                                                    "printing\x0a"
                                                                )
                                                            ]
                                                        )
                                                    )
                                                ]
                                            )
                                        )
                                    ),
                                    new FunctionDeclaration
                                    (
                                        "print",
                                        new FunctionType
                                        (
                                            "FZv",
                                            [],
                                            new BasicType(TypeKind.void_, "v"),
                                            VariadicType.nonVariadic,
                                            Linkage.d
                                        ),
                                        new CompoundStatement
                                        (
                                            [
                                                new ExpressionStatement
                                                (
                                                    new CallExpression
                                                    (
                                                        new BasicType(TypeKind.int32, "i"),
                                                        new VariableExpression
                                                        (
                                                            new FunctionType
                                                            (
                                                                "UxPaYi",
                                                                [
                                                                    new Parameter
                                                                    (
                                                                        0LU,
                                                                        new PointerType
                                                                        (
                                                                            "xPa",
                                                                            new BasicType(TypeKind.char_, "xa")
                                                                        ),
                                                                        null,
                                                                        null
                                                                    )
                                                                ],
                                                                new BasicType(TypeKind.int32, "i"),
                                                                VariadicType.untyped,
                                                                Linkage.c
                                                            ),
                                                            new FunctionDeclaration
                                                            (
                                                                "printf",
                                                                new FunctionType
                                                                (
                                                                    "UxPaYi",
                                                                    [
                                                                        new Parameter
                                                                        (
                                                                            0LU,
                                                                            new PointerType
                                                                            (
                                                                                "xPa",
                                                                                new BasicType(TypeKind.char_, "xa")
                                                                            ),
                                                                            null,
                                                                            null
                                                                        )
                                                                    ],
                                                                    new BasicType(TypeKind.int32, "i"),
                                                                    VariadicType.untyped,
                                                                    Linkage.c
                                                                ),
                                                                null
                                                            )
                                                        ),
                                                        new FunctionDeclaration
                                                        (
                                                            "printf",
                                                            new FunctionType
                                                            (
                                                                "UxPaYi",
                                                                [
                                                                    new Parameter
                                                                    (
                                                                        0LU,
                                                                        new PointerType
                                                                        (
                                                                            "xPa",
                                                                            new BasicType(TypeKind.char_, "xa")
                                                                        ),
                                                                        null,
                                                                        null
                                                                    )
                                                                ],
                                                                new BasicType(TypeKind.int32, "i"),
                                                                VariadicType.untyped,
                                                                Linkage.c
                                                            ),
                                                            null
                                                        ),
                                                        [
                                                            new StringExpression
                                                            (
                                                                new PointerType
                                                                (
                                                                    "xPa",
                                                                    new BasicType(TypeKind.char_, "xa")
                                                                ),
                                                                "printing\x0a"
                                                            )
                                                        ]
                                                    )
                                                )
                                            ]
                                        )
                                    ),
                                    []
                                )
                            )
                        ]
                    ),
                    "__lambda2"
                )
            );


    /*return new FunctionExpression
    (
        PointerType
        (
            FunctionType
            (
                [],
                BasicType(TypeKind.void_),
                VariadicType.nonVariadic,
                Linkage.d
            )
        ),
        FunctionLiteralDeclaration
        (
            FunctionType
            (
                [],
                BasicType(TypeKind.void_)
            ),
            NodeType.reserved,
            CompoundStatement
            (
                [
                    ExpressionStatement
                    (
                        CallExpression
                        (
                            BasicType(TypeKind.void_),
                            VariableExpression
                            (
                                FunctionType
                                (
                                    [],
                                    BasicType(TypeKind.void_),
                                    VariadicType.nonVariadic,
                                    Linkage.d
                                ),
                                FunctionDeclaration
                                (
                                    "print",
                                    FunctionType
                                    (
                                        [],
                                        BasicType(TypeKind.void_)
                                    ),
                                    CompoundStatement
                                    (
                                        [
                                            ExpressionStatement
                                            (
                                                CallExpression
                                                (
                                                    BasicType(TypeKind.int32),
                                                    VariableExpression
                                                    (
                                                        FunctionType
                                                        (
                                                            [
                                                                Parameter
                                                                (
                                                                    0,
                                                                    PointerType
                                                                    (
                                                                        BasicType(TypeKind.char_)
                                                                    ),
                                                                    null
                                                                )
                                                            ],
                                                            BasicType(TypeKind.int32),
                                                            VariadicType.untyped,
                                                            Linkage.c
                                                        ),
                                                        FunctionDeclaration
                                                        (
                                                            "printf",
                                                            FunctionType
                                                            (
                                                                [
                                                                    Parameter
                                                                    (
                                                                        0,
                                                                        PointerType
                                                                        (
                                                                            BasicType(TypeKind.char_)
                                                                        ),
                                                                        null
                                                                    )
                                                                ],
                                                                BasicType(TypeKind.int32),
                                                                VariadicType.untyped,
                                                                Linkage.c
                                                            )
                                                        )
                                                    ),
                                                    FunctionDeclaration
                                                    (
                                                        "printf",
                                                        FunctionType
                                                        (
                                                            [
                                                                Parameter
                                                                (
                                                                    0,
                                                                    PointerType
                                                                    (
                                                                        BasicType(TypeKind.char_)
                                                                    ),
                                                                    null
                                                                )
                                                            ],
                                                            BasicType(TypeKind.int32),
                                                            VariadicType.untyped,
                                                            Linkage.c
                                                        )
                                                    ),
                                                    [
                                                        StringExpression
                                                        (
                                                            PointerType
                                                            (
                                                                BasicType(TypeKind.char_)
                                                            ),
                                                            "printing\n"
                                                        )
                                                    ]
                                                )
                                            )
                                        ]
                                    )
                                )
                            ),
                            FunctionDeclaration
                            (
                                "print",
                                FunctionType
                                (
                                    [],
                                    BasicType(TypeKind.void_)
                                ),
                                CompoundStatement
                                (
                                    [
                                        ExpressionStatement
                                        (
                                            CallExpression
                                            (
                                                BasicType(TypeKind.int32),
                                                VariableExpression
                                                (
                                                    FunctionType
                                                    (
                                                        [
                                                            Parameter
                                                            (
                                                                0,
                                                                PointerType
                                                                (
                                                                    BasicType(TypeKind.char_)
                                                                ),
                                                                null
                                                            )
                                                        ],
                                                        BasicType(TypeKind.int32),
                                                        VariadicType.untyped,
                                                        Linkage.c
                                                    ),
                                                    FunctionDeclaration
                                                    (
                                                        "printf",
                                                        FunctionType
                                                        (
                                                            [
                                                                Parameter
                                                                (
                                                                    0,
                                                                    PointerType
                                                                    (
                                                                        BasicType(TypeKind.char_)
                                                                    ),
                                                                    null
                                                                )
                                                            ],
                                                            BasicType(TypeKind.int32),
                                                            VariadicType.untyped,
                                                            Linkage.c
                                                        )
                                                    )
                                                ),
                                                FunctionDeclaration
                                                (
                                                    "printf",
                                                    FunctionType
                                                    (
                                                        [
                                                            Parameter
                                                            (
                                                                0,
                                                                PointerType
                                                                (
                                                                    BasicType(TypeKind.char_)
                                                                ),
                                                                null
                                                            )
                                                        ],
                                                        BasicType(TypeKind.int32),
                                                        VariadicType.untyped,
                                                        Linkage.c
                                                    )
                                                ),
                                                [
                                                    StringExpression
                                                    (
                                                        PointerType
                                                        (
                                                            BasicType(TypeKind.char_)
                                                        ),
                                                        "printing\n"
                                                    )
                                                ]
                                            )
                                        )
                                    ]
                                )
                            )
                        )
                    )
                ]
            ),
            "__lambda2",
        )
    );*/
}

// macro foo(AstNode n)
// {
//     return n;
// }


void bar(ExpressionStatement e)
{

}

void function() asd()
{
    return {
        print();
    };
}


void main()
{
    void function() bbbb1 = foo(4 + 5);
    bbbb1();
    // void function() bbbb = asd();

    // int bar = 3;
    // auto a = foo(4 + 5);
    // println(a);
    // toDecl(4 + 5);
    // println(bar);
    // println("ok");
}

