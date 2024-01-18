extern int global;

void set_global(int x);

void main() {
    set_global(10);
    print_endline("I'm main and global is");
    print(global);
}