
struct A
{
    int a;
    int b;
};

struct A mk_str(int a, int b)
{
    struct A str;
    str.a = a;
    str.b = b;
    return str;
}

int fct(struct A str, struct A str2)
{
	return str.a;
}

int main()
{
	putchar('0' + fct(mk_str(1,2), mk_str(3,4)));
	putchar(10);
	return 0;
}

