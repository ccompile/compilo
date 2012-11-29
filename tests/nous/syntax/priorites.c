
int main()
{
    int a, b, c;
    a = c || a = b;
    a = c = a || b;
    a = c && a = b;
    a = a = b && c;
    a = b && b == c;
    a = c + a || b;
    a = a - b + c;
    a = a + b * c;
    a = a * c ++;
    a = b + c [0];
    a = b.x * a;

    return 0;
}


