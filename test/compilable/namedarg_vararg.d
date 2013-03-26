import core.vararg;
import std.stdio;

void test(A...)(A a)
{
    foreach(t; a)
        writeln(t);
}

void main ()
{
    test(x:33.3,
         z: 44.4,
         a: 9999,
         b: 8888,
         7777,
         d:"Yehaw");
}
