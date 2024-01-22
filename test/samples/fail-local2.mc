int foo(int a, bool b)
{
  int c;
  bool d; // Now fails because unused

  c = a;

  return c + 10;
}

int main() {
 print(foo(37, false));
 return 0;
}
