/**
 * Compiler implementation of the
 * $(LINK2 http://www.dlang.org, D programming language).
 *
 * Copyright:   Copyright (C) 1999-2019 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 http://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/serializer.d, _serializer.d)
 * Documentation:  https://dlang.org/phobos/dmd_serializer.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/serializer.d
 */

module dmd.root.serializer;

import core.internal.traits;
import core.stdc.config;
import core.stdc.stdarg;
import core.stdc.string;

import dmd.root.array;
import dmd.root.outbuffer;

string serialize(T)(auto ref T value)
{
    OutBuffer buffer;
    Serialzier(&buffer).serialize(value);

    return cast(string) buffer.extractSlice;
}

struct Serialzier
{
    private
    {
        enum Type
        {
            none,
            basic,
            string,
            singleLineArray,
            multilineArray,
            associativeArray,
            enum_,
            pointer,
            struct_,
            class_,
            union_
        }

        enum singleLineArrayLimit = 5;
        enum indentation = 2;
        int level = 0;
        bool shouldIdent;
        bool isTopLevel = true;
        Type previousType = Type.none;
        OutBuffer* buffer;
        int references;
        int[void*] serializedReferences;
        bool[void*] customized; // should be a set
    }

    this(OutBuffer* buffer)
    {
        this.buffer = buffer;
        append("---");
    }

    void serialize(T)(ref T value)
    {
        const isTopLevel = this.isTopLevel;
        this.isTopLevel = false;

        static if (isBasicType!T)
            serializeBasicType(value, isTopLevel);
        else static if (isString!T) // needs to come before `isArray`
            serializeString(value, isTopLevel);
        else static if (isArray!T)
            serializeArray(value, isTopLevel);
        else static if (isAssociativeArray!T)
            serializeAssociativeArray(value, isTopLevel);
        else static if (is(T == enum))
            serializeEnum(value, isTopLevel);
        else static if (isPointer!T)
            serializePointer(value, isTopLevel);
        else static if (is(T == struct))
            serializeStruct(value, isTopLevel);
        else static if (is(T == class))
            serializeClass(value, isTopLevel);
        else static if (is(T == union))
            serializeUnion(value, isTopLevel);
        else
            static assert(false, "Serializing a value of type `" ~ T.stringof ~ "` is not supported");
    }

private:

    void serializeBasicType(T)(T value, bool isTopLevel)
    if (isBasicType!T)
    {
        previousType = Type.basic;

        if (isTopLevel)
            append(' ');

        append(value);
    }

    void serializeString(T)(T value, bool isTopLevel)
    if (isString!T)
    {
        alias U = Unqual!T;
        previousType = Type.string;

        if (isTopLevel)
            append(' ');

        static if (is(U == string))
            append(value);
        else
        {
            foreach (e; value)
                append(e);
        }
    }

    void serializeArray(T)(ref T value, bool isTopLevel)
    if (isArray!T)
    {
        alias ElementType = ArrayTypeOf!T;

        const isSingleLineArray = this.isSingleLineArray(value);
        const previousType = this.previousType;
        this.previousType = isSingleLineArray ? Type.singleLineArray :
            Type.multilineArray;

        if (isSingleLineArray)
        {
            if (isTopLevel)
                append(' ');

            append('[');

            foreach (i, ref e; value)
            {
                if (i != 0)
                    append(", ");

                serialize(e);
            }

            append(']');
        }

        else
        {
            if (previousType != Type.associativeArray)
                newline();

            foreach (i, ref e; value)
            {
                if (i != 0)
                    newline();

                if (previousType == Type.associativeArray)
                {
                    append("- ");
                    serialize(e);
                }

                else
                {
                    indent(isTopLevel, {
                        append("- ");
                        serialize(e);
                    });
                }
            }
        }
    }

    void serializeAssociativeArray(T)(T value, bool isTopLevel)
    if (isAssociativeArray!T)
    {
        alias Key = KeyType!T;
        enum isComplexKey = !isBasicType!Key && !isString!Key;

        if (value.length == 0)
        {
            if (isTopLevel)
                append(' ');

            append("{}");
            return;
        }

        newline();
        size_t i = 0;

        foreach (ref t; value.byKeyValue)
        {
            if (i != 0)
                newline();

            static if (isComplexKey)
            {
                previousType = Type.associativeArray;
                append("? ");

                indent({
                    serialize(t.key);
                });

                newline();
                append(": ");

                previousType = Type.associativeArray;

                indent({
                    serialize(t.value);
                });

            }
            else
            {
                previousType = Type.associativeArray;
                serialize(t.key);
                append(": ");
                previousType = Type.associativeArray;
                serialize(t.value);
            }

            i++;
        }
    }

    void serializeEnum(T)(ref T value, bool isTopLevel) if (is(T == enum))
    {
        previousType = Type.enum_;

        if (isTopLevel)
            append(' ');

        append(value.enumMemberName);
    }

    void serializePointer(T)(T value, bool isTopLevel) if (isPointer!T)
    {
        alias Target = Unqual!(PointerTarget!T);

        previousType = Type.pointer;

        if (isTopLevel)
            append(' ');

        if (value is null)
        {
            append("null");
            return;
        }

        static if (is(Target == char))
        {
            const str = value[0 .. strlen(value)];
            serialize(str);
        }

        else static if (is(Target == void))
            appendf("%p", value);
        else static if (!isOpaque!Target)
            serialize(*value);
    }

    void serializeStruct(T)(ref T value, bool isTopLevel)
    if (is(T == struct))
    {
        const previousType = this.previousType;
        this.previousType = Type.struct_;

        if (isTopLevel)
            append(' ');

        static if (implementsCustomSerialization!T)
            value.serialize(this);
        else
        {
            append("!d/struct:", __traits(identifier, T));
            newline();

            if (previousType == Type.associativeArray)
                serializeFields(value);
            else
            {
                indent(isTopLevel, {
                    serializeFields(value);
                });
            }
        }
    }

    void serializeClass(T)(T value, bool isTopLevel)
    if (is(T == class))
    {
        previousType = Type.class_;

        static if (isDClass!T)
            const className = value.classinfo.name;
        else
            enum className = __traits(identifier, T);

        if (value is null)
        {
            if (isTopLevel)
                append(' ');

            append("null");
            return;
        }

        static if (implementsCustomSerialization!T)
        {{
            const ptr = cast(const(void)*) value;

            if (ptr !in customized)
            {
                customized[cast(const(void)*) ptr] = true;
                this.isTopLevel = isTopLevel;
                value.serialize(this);
                return;
            }
        }}

        if (isTopLevel)
            append(' ');

        if (const reference = cast(const(void)*) value in serializedReferences)
        {
            append('*');
            append(*reference);
            return;
        }

        const reference = ++references;
        serializedReferences[cast(const(void)*) value] = reference;
        append("&");
        append(reference);
        append(' ');
        append("!d/class:", className);
        newline();

        if (previousType == Type.associativeArray)
        {
            serializeBaseClass(value);
            serializeFields(value);
        }
        else
        {
            indent(isTopLevel, {
                serializeBaseClass(value);
                serializeFields(value);
            });
        }
    }

    void serializeUnion(T)(ref T value, bool isTopLevel)
    if (is(T == union))
    {
        previousType = Type.union_;

        if (isTopLevel)
            append(' ');

        append("!d/union:", __traits(identifier, T));
        newline();
        indent(isTopLevel, { serializeFields(value); });
    }

    void serializeBaseClass(T)(T value) if (is(T == class))
    {
        alias Base = BaseTypeTuple!T;
        Base baseValue = value;

        static if (!is(Unqual!Base == .object.Object) && Base.length > 0)
        {
            serializeBaseClass(baseValue);
            serializeFields(baseValue);

            static if (Base[0].tupleof.length > 0)
                newline();
        }
    }

    void serializeFields(T)(ref T value)
    if (is(T == class) || is(T == struct) || is(T == union))
    {
        foreach (i, _; typeof(T.tupleof))
        {
            alias FieldType = typeof(T.tupleof[i]);
            enum name = __traits(identifier, T.tupleof[i]);

            if (i != 0)
                newline();

            append(name, ":");

            if (!isMultiline(value.tupleof[i]))
                append(' ');

            serialize(value.tupleof[i]);
        }
    }

    bool isSingleLineArray(T)(const ref T value) if (isArray!T)
    {
        alias ElementType = ArrayTypeOf!T;
        return value.length == 0 ||
            (isBasicType!ElementType && singleLineArrayLimit >= value.length);
    }

    bool isMultiline(T)(const ref T value)
    {
        static if (isString!T)
            return false;
        else static if (isArray!T)
            return !isSingleLineArray(value);
        else static if (isPointer!T)
        {
            alias Target = PointerTarget!T;

            static if (isOpaque!Target || is(Unqual!Target == void))
                return false;
            else
                return value ? isMultiline(*value) : false;
        }
        else
            return false;
    }

    void curlyBraces(void delegate() pure @nogc @safe block)
    {
        bracket('{', '}', block);
    }

    void bracket(char start, char end, void delegate() pure @nogc @safe block)
    {
        newline();
        append(start);
        newline();
        indent(block);
        newline();
        append(end);
    }

    void indent(void delegate() block)
    {
        level++;
        scope (exit) level--;
        block();
    }

    void indent(bool isTopLevel, void delegate() block)
    {
        if (!isTopLevel)
            level++;

        scope (exit)
        {
            if (!isTopLevel)
                level--;
        }

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

    void append(string[] strings ...)
    {
        indent();

        foreach (str ; strings)
            buffer.writestring(str);

        shouldIdent = false;
    }

    void append(T)(T value)
    {
        alias U = Unqual!T;

        static const(char)* printfFormatter(T)()
        {
            static if (is(U == byte) || is(U == ubyte))
                return "%d";
            else static if (is(U == char))
                return "%c";
            else static if (is(U == short) || is (U == int))
                return "%d";
            else static if (is(U == ushort) || is (U == uint))
                return "%u";
            else static if (is(U == long))
                return "%lld";
            else static if (is(U == ulong))
                return "%llu";
            else static if (is(U == float) || is(U == double))
                return "%g";
            else static if (is(U == real))
                return "%Lg";
            else
                static assert(false, "Serializing a value of type `" ~ U.stringof ~ "` is not supported");
        }

        indent();

        static if (is(U == bool))
            buffer.writestring(value ? "true" : "false");
        else static if (is(U == char) || is(U == wchar) || is(U == dchar))
            buffer.writeUTF8(value);
        else
            buffer.printf(printfFormatter!U, value);

        shouldIdent = false;
    }

    extern (C++) void appendf(const(char)* format, ...)
    {
        indent();
        va_list ap;
        va_start(ap, format);
        buffer.vprintf(format, ap);
        va_end(ap);
        shouldIdent = false;
    }
}

private:

template isBasicType(T)
{
    alias U = Unqual!T;

    enum isBasicType =
        !isAggregateType!T &&
        is(U == bool) ||
        is(U == char) ||
        is(U == wchar) ||
        is(U == dchar) ||
        is(U == byte) ||
        is(U == ubyte) ||
        is(U == short) ||
        is(U == ushort) ||
        is(U == int) ||
        is(U == uint) ||
        is(U == long) ||
        is(U == ulong) ||
        is(U == float) ||
        is(U == double) ||
        is(U == real);
}

template isString(T)
{
    static if (isArray!T && !isAggregateType!T)
    {
        alias ElementType = Unqual!(ArrayTypeOf!T);

        enum isString =
            is(ElementType == char) ||
            is(ElementType == wchar) ||
            is(ElementType == dchar);
    }
    else
        enum isString = false;
}

template isNullable(T)
{
    alias U = Unqual!T;
    enum isNullable = isPointer!U || is(U == class) || is(U == interface);
}

enum isAggregateType(T) =
    is(T == union) ||
    is(T == class) ||
    is(T == struct) ||
    is(T == interface);

enum isPointer(T) = is(T == U*, U) && !isAggregateType!T;
enum isArray(T) = is(ArrayTypeOf!T) && !isAggregateType!T;
enum isAssociativeArray(T) = __traits(isAssociativeArray, T) && !isAggregateType!T;
enum isOpaque(T) = !is(typeof(T.sizeof)) && (is(T == struct) || is(T == union));

enum implementsCustomSerialization(T) =
    __traits(hasMember, T, "serialize") &&
    __traits(compiles, { Serialzier s; T.init.serialize(s); });

// Due to the host compiler on the CI environments it's not possible to identify
// the exact kind of class, because `__traits(getLinkage)` is implemented in
// later versions of the compiler. It's enough to identify if it's a D class or
// not, but ideally we would like that serializing an Objective-C class should
// result in an error.
template isDClass(T)
{
    alias BaseTypes = BaseTypeTuple!T;

    static if (is(T == object.Object))
        enum isDClass = true;
    else static if (BaseTypes.length == 0)
        enum isDClass = false;
    else
        enum isDClass = isDClass!(BaseTypes[0]);
}

template ArrayTypeOf(T)
{
    alias U = Unqual!T;

    static if (is(U : E[], E))
        alias ArrayTypeOf = E;
    else
        static assert(false, T.stringof ~ " is not an array");
}

alias PointerTarget(T : T*) = T;

template BaseTypeTuple(A)
{
    static if (is(A P == super))
        alias BaseTypeTuple = P;
    else
        static assert(0, "argument is not a class or interface");
}

string enumMemberName(T)(const ref T value)
{
    foreach (name; __traits(allMembers, T))
    {
        if (__traits(getMember, T, name) == value)
            return name;
    }

    assert(0);
}

alias KeyType(V : V[K], K) = K;
alias ValueType(V : V[K], K) = V;

@("bool, true")
unittest
{
    const result = true.serialize();
    assert(result == "--- true", result);
}

@("bool, false")
unittest
{
    const result = false.serialize();
    assert(result == "--- false", result);
}

@("char")
unittest
{
    const result = 'a'.serialize();
    assert(result == "--- a", result);
}

@("wchar")
unittest
{
    const result = 'ö'.serialize();
    assert(result == "--- ö", result);
}

@("dchar")
unittest
{
    const result = '🍺'.serialize();
    assert(result == "--- 🍺", result);
}

@("byte")
unittest
{
    const byte value = 3;
    const result = value.serialize();

    assert(result == "--- 3", result);
}

@("ubyte")
unittest
{
    const ubyte value = 3;
    const result = value.serialize();

    assert(result == "--- 3", result);
}

@("short")
unittest
{
    const short value = 3;
    const result = value.serialize();

    assert(result == "--- 3", result);
}

@("short")
unittest
{
    const ushort value = 3;
    const result = value.serialize();

    assert(result == "--- 3", result);
}

@("int")
unittest
{
    const result = serialize(3);
    assert(result == "--- 3", result);
}

@("uint")
unittest
{
    const result = serialize(3u);
    assert(result == "--- 3", result);
}

@("long")
unittest
{
    const result = serialize(10_000_000_000);
    assert(result == "--- 10000000000", result);
}

@("ulong")
unittest
{
    const result = serialize(10_000_000_000u);
    assert(result == "--- 10000000000", result);
}

@("double")
unittest
{
    const result = serialize(3.1);
    assert(result == "--- 3.1", result);
}

@("float")
unittest
{
    const result = serialize(3.1f);
    assert(result == "--- 3.1", result);
}

@("real")
unittest
{
    const result = serialize(3.1L);
    assert(result == "--- 3.1", result);
}

@("string")
unittest
{
    const result = "foo".serialize();
    assert(result == "--- foo", result);
}

@("wstring")
unittest
{
    const result = "öl"w.serialize();
    assert(result == "--- öl", result);
}

@("dstring")
unittest
{
    const result = "🍺🍺"d.serialize();
    assert(result == "--- 🍺🍺", result);
}

@("char[]")
unittest
{
    const result = "foo".dup.serialize();
    assert(result == "--- foo", result);
}

@("wchar[]")
unittest
{
    const result = "öl"w.dup.serialize();
    assert(result == "--- öl", result);
}

@("dchar[]")
unittest
{
    const result = "🍺🍺"d.dup.serialize();
    assert(result == "--- 🍺🍺", result);
}

@("C string, char*")
unittest
{
    const result = "foo".ptr.serialize();
    assert(result == "--- foo", result);
}

@("array")
unittest
{
    const result = [1, 2, 3].serialize();
    assert(result == "--- [1, 2, 3]", result);
}

@("empty array")
unittest
{
    string[] array;
    const result = array.serialize();
    assert(result == "--- []", result);
}

@("array with more than five elements")
unittest
{
    enum expected = "---
- 1
- 2
- 3
- 4
- 5
- 6";

    const result = [1, 2, 3, 4, 5, 6].serialize();
    assert(result == expected, result);
}

@("dmd.root.array.Array")
unittest
{
    enum initiailzeSize = 3;

    auto array = Array!int(initiailzeSize);

    foreach (i; 0 .. initiailzeSize)
        array[i] = i + 1;

    const result = serialize(array);
    assert(result == "--- [1, 2, 3]", result);
}

@("pointer")
unittest
{
    int a = 3;
    const result = serialize(&a);
    assert(result == "--- 3", result);
}

@("void pointer")
unittest
{
    int a = 3;
    void* ptr = &a;

    OutBuffer buffer;
    buffer.printf("%p", ptr);

    const result = serialize(ptr);
    assert(result == "--- " ~ buffer.peekSlice, result);
}

@("struct")
unittest
{
    static struct Foo
    {
        int foo = 3;
        int bar = 4;
    }

    enum expected = "--- !d/struct:Foo
foo: 3
bar: 4";

    const result = Foo().serialize();
    assert(result == expected, result);
}

@("struct containing struct")
unittest
{
    static struct Bar
    {
        int bar = 4;
    }

    static struct Foo
    {
        int foo = 3;
        Bar bar;
    }

    enum expected = "--- !d/struct:Foo
foo: 3
bar: !d/struct:Bar
  bar: 4";

    const result = Foo().serialize();
    assert(result == expected, result);
}

@("class")
unittest
{
    static class Foo
    {
        int foo = 3;
        int bar = 4;
    }

    const expected = "--- &1 !d/class:" ~ Foo.classinfo.name ~ "
foo: 3
bar: 4";

    scope foo = new Foo;
    const result = foo.serialize();
    assert(result == expected, result);
}

@("C++ class")
unittest
{
    extern (C++) static class Foo
    {
        int foo = 3;
        int bar = 4;
    }

    enum expected = "--- &1 !d/class:Foo
foo: 3
bar: 4";

    scope foo = new Foo;
    const result = foo.serialize();
    assert(result == expected, result);
}

// Due to the host compiler on the CI environments it's not possible to identify
// an Objective-C class, because `__traits(getLinkage)` is implemented in later
// versions of the compiler.
// @("Objective-C class")
// unittest
// {
//     extern (Objective-C) static class Foo
//     {
//         int foo = 3;
//         int bar = 4;
//     }
//
//     assert(!__traits(compiles, Foo.init.serialize()));
// }

@("class with base classes")
unittest
{
    static class A
    {
        int a = 3;
        int b = 4;
    }

    static class B : A {}

    static class C : B
    {
        int c = 5;
        int d = 6;
    }

    static class D : C
    {
        int e = 7;
        int f = 8;
    }

    const expected = "--- &1 !d/class:" ~ D.classinfo.name ~ "
a: 3
b: 4
c: 5
d: 6
e: 7
f: 8";

    scope bar = new D;
    const result = bar.serialize();
    assert(result == expected, result);
}

version (unittest)
{
    static class CircleReferenceA
    {
        CircleReferenceB b;
    }

    static class CircleReferenceB
    {
        CircleReferenceA a;
    }
}

@("class with circle reference")
unittest
{
    const expected = "--- &1 !d/class:" ~ CircleReferenceA.classinfo.name ~ "
b: &2 !d/class:" ~ CircleReferenceB.classinfo.name ~ "
  a: *1";

    scope a = new CircleReferenceA;
    scope b = new CircleReferenceB;

    a.b = b;
    b.a = a;

    const result = a.serialize();
    assert(result == expected, result);
}

version (unittest)
{
    class Visitor
    {
        Serialzier* serializer;

        this(Serialzier* serializer)
        {
            this.serializer = serializer;
        }

        void visit(CustomFoo foo)
        {
            serializer.serialize(foo);
        }

        void visit(CustomBar bar)
        {
            serializer.serialize(bar);
        }
    }

    class CustomFoo
    {
        int a = 3;

        void serialize(ref Serialzier serializer)
        {
            scope visitor = new Visitor(&serializer);
            accept(visitor);
        }

        void accept(Visitor v)
        {
            v.visit(this);
        }
    }

    class CustomBar : CustomFoo
    {
        int b = 4;

        override void accept(Visitor v)
        {
            v.visit(this);
        }
    }
}

@("class with custom serialization")
unittest
{
    const expected = "--- &1 !d/class:" ~ CustomBar.classinfo.name ~ "
a: 3
b: 4";

    scope CustomFoo foo = new CustomBar;
    // Serialzier s;
    // foo.serialize(s);
    const result = serialize(foo);
    assert(result == expected, result);
}

@("enum")
unittest
{
    enum Foo
    {
        a,
        b
    }

    const result = Foo.b.serialize();
    assert(result == "--- b", result);
}

@("union")
unittest
{
    union Foo
    {
        int a;
        char b;
    }

    enum expected = "--- !d/union:Foo
a: 97
b: a";

    const result = Foo(97).serialize();
    assert(result == expected, result);
}

@("associative array")
unittest
{
    enum expected = "---
2: bar
1: foo";

    const result = [1: "foo", 2: "bar"].serialize();
    assert(result == expected, result);
}

@("associative array with array as key type")
unittest
{
    // The order of an associative array is implementation defined.
    // The ho
    enum expected1 = "---
? [4, 5, 6]
: bar
? [1, 2, 3]
: foo";

    enum expected2 = "---
? [1, 2, 3]
: foo
? [4, 5, 6]
: bar";

    enum value = [
        [1, 2, 3]: "foo",
        [4, 5, 6]: "bar"
    ];

    const result = value.serialize();
    assert(result == expected1 || result == expected2, result);
}

@("associative array with array as key and complex values")
unittest
{
    enum expected = "---
? - 1
  - 2
  - 3
  - 4
  - 5
  - 6
: - 7
  - 8
  - 9
  - 10
  - 11
  - 12";

    enum value = [
        [1, 2, 3, 4, 5, 6]: [7, 8, 9, 10, 11, 12]
    ];

    const result = value.serialize();
    assert(result == expected, result);
}

@("associative array with struct as key")
unittest
{
    static struct Foo
    {
        int foo;
        int[] bar = [1, 2, 3, 4, 5, 6];
    }

    enum expected = "---
? !d/struct:Foo
  foo: 4
  bar:
    - 1
    - 2
    - 3
    - 4
    - 5
    - 6
: bar
? !d/struct:Foo
  foo: 3
  bar:
    - 1
    - 2
    - 3
    - 4
    - 5
    - 6
: foo";

    enum value = [
        Foo(3): "foo",
        Foo(4): "bar"
    ];

    const result = value.serialize();
    assert(result == expected, result);
}

@("empty associative array")
unittest
{
    int[int] value;
    const result = value.serialize();
    assert(result == "--- {}", result);
}
