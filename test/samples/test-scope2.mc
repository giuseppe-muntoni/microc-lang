void foo() {
    print_endline("I'm foo");
}

void main() {
    foo();

    {
        int foo;
        foo = 5;
        print(foo);
    }

}