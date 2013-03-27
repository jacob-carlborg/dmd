
void test (int a, 
           int _I_dont_care_what_I_am_named_at_caller_site,
	   int _starting_underscore_turns_off_namedarg_checking = 1,
	   double _ok = 55.5)
{

}

void main ()
{
    test(a: 1111,
         b: 2222);

    test(a: 1111,
         anything: 2222);

    test(a: 1111,
         b: 2222,
	 c: 3333);

}
