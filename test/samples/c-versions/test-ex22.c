// micro-C example 22 -- leapyear, optimization of andalso and orelse
#include<stdio.h>

int leapyear(int y) {
  return y % 4 == 0 && (y % 100 != 0 || y % 400 == 0);
}

int main() {
  int y;
  int n;

  y = 1889;
  n = 2020;
  
  while (y < n) {
    y = y + 1;
    if (leapyear(y))
      printf("%d\n", y);
  }
}