
void incr(int* a)
{
    (*a)++;
}

int main()
{
    int a, b, c;

    incr(&a);

    return 0;
}

