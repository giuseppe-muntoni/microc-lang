void foo() {}

int bar(int a, bool b, int c) { 
  b = true;
  return a + c;
}

int main()
{
  print(bar(17, false, 25));
  return 0;
}
