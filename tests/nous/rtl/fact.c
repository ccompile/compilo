
int fact(int n)
{
    int resultat, i;
    resultat = 1;
    i = 2;

    while(i <= n)
    {
        resultat = resultat * i;
        ++i;
    }

    return resultat;
}

int print_num_renverse(int n)
{
    while(n)
    {
        putchar('0' + (n % 10));
        n = n / 10;
    }
}

int main()
{
    print_num_renverse(fact(5));
    return 0;
}

int dummy_fun()
{
    return 0;
}
