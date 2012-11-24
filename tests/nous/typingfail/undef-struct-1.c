
struct A
{
    struct B* pair;
};

struct B
{
    struct A* pair;
};

int main()
{
    putchar(10);
    return 0;
}

