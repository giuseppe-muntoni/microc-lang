int foo() {
    int x;
    x = 0;
    if (x > 0)
        return x;
    else
        x = x + 1;
}

int main() {
    foo();
}