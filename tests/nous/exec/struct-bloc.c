
struct A
{
    int a;
    int b;
};

void print_obj(struct A* obj)
{
     putchar('0' + obj->a);
     putchar('0' + obj->b);
     putchar(10);
}

int main()
{
    {
	struct A obj;
	obj.a = 1;
	obj.b = 2;

	print_obj(&obj);
    }

    {
	struct A obj;

	print_obj(&obj);

	obj.a = 3;
	obj.b = 4;

	print_obj(&obj);
    }

    return 0;
}


