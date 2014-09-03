struct S(alias func)
{
    void call()
    {
        func();
    }
}

extern(C) int printf(in char*, ...);

void f(int i = 77)
{
    void g()
    {
        printf("i = %d;\n", i);
        assert(i == 77);
    }
    S!g().call();
}
