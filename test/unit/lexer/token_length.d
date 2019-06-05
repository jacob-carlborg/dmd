module lexer.token_length;

import std.stdio;

import dmd.tokens : Token;

import support : afterEach;

@afterEach deinitializeFrontend()
{
    import dmd.frontend : deinitializeDMD;
    deinitializeDMD();
}

@("single 0")
unittest
{
    enum code = "0";
    assert(firstToken(code).length == code.length);
}

@("single non-zero digit")
unittest
{
    enum code = "2";
    assert(firstToken(code).length == code.length);
}

@("multiple digits")
unittest
{
    enum code = "123";
    assert(firstToken(code).length == code.length);
}

@("leading zeros and multiple digits")
unittest
{
    enum code = "00123";
    assert(firstToken(code).length == code.length);
}

@("character literal")
unittest
{
    enum code = "'a'";
    assert(firstToken(code).length == code.length);
}

@("character literal with newline escape sequence")
unittest
{
    enum code = `'\n'`;
    assertLengthOfFirstToken(code, code.length);
}

@("wysiwyg string literal")
unittest
{
    // assertLengthOfFirstToken(`r"foo"`);
}

@("backquoted wysiwyg string literal")
unittest
{
    assertLengthOfFirstToken("`foo`");
}

@("hex string literal")
unittest
{
    assertLengthOfFirstToken(`x"0A"`);
}

@("delimited string literal")
unittest
{
    assertLengthOfFirstToken(`q"(foo)"`);
}

@("token string literal")
unittest
{
    assertLengthOfFirstToken(`q{foo}`);
}

@("string literal")
unittest
{
    assertLengthOfFirstToken(`"foo"`);
}

@("identifier")
unittest
{
    assertLengthOfFirstToken("foo");
}

@("__DATE__")
unittest
{
    assertLengthOfFirstToken("__DATE__");
}

private:

void assertLengthOfFirstToken(string code, size_t length = size_t.max)
{
    import std.conv : text;

    if (length == size_t.max)
        length = code.length;

    const token = firstToken(code);
    assert(token.length == length, token.length.text);
}

Token firstToken(string code)
{
    import dmd.lexer : Lexer;

    import support : NoopDiagnosticReporter;

    scope reporter = new NoopDiagnosticReporter;
    scope lexer = new Lexer("test", code.ptr, 0, code.length, false, true, reporter);
    lexer.nextToken();

    return lexer.token;
}
