#include<stdio.h>

int main() {
  int y;
  int n;
  y = 1889;
  n = 2020;
  while (y < n) {
    y = y + 1;
    if (y % 4 == 0 && (y % 100 != 0 || y % 400 == 0)) 
      printf("%d\n", y);
  }
}