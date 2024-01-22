/* Bug noticed by Pin-Chin Huang */

int fun(int x, int y)
{
  // Just to avoid error on unused params
  x = 0;
  y = 0;
  return 0;
}

int main()
{
  int i;
  i = 1;

  fun(i = 2, i = i+1);

  print(i);
  return 0;
}

