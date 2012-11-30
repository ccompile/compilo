
int main()
{
    int a, b, c;
    a = b = c;
    a = c = a || b;
    a = a = b && c;
    a = b && b == c;
    a = c + a || b;
    a = a - b + c;
    a = a + b * c;
    a = a * c ++;
    a++;
    a = a % b * c;
    a = b * c % a;
    a = a % b + c;
    a = - b;
    a = - + b;
    a = - a + b; // doit être (-a) + b
    a = - b % c; // on s'en fiche, car (-b) % c == -(b % c) (pas en maths, en C !)
    a = - b ++;

    return 0;
}


