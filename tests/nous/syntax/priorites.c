
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
    a++;
//    --a++;
//    a++++;

    return 0;
}


