void printb(bool val)
{
  if(val)
    print(1);
  else
    print(0);
}

int main()
{
  print_float(9.9900 + 2.15);
  print_float(3.0 - 1.5);
  print_float(1.1 * 2);
  print_float(100.45 / 2.34);
  print_float(99.99999);
  printb(1.1 == 2.2);
  printb(0.99999999 == 1);
  printb(0.999 == 1);
  print_float(99.99999);
  printb(1.1 != 2.2);
  printb(1.85 != 1.85);
  print_float(99.99999);
  printb(1.1 < 2.2);
  printb(2.2 < 1.1);
  print_float(99.99999);
  printb(1.9 <= 2);
  printb(1.1 <= 1);
  return 0;
}