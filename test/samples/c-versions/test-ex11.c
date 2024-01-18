// micro-C example 11 -- n queens problem * 1996-12-20, 2009-10-01
#include<stdio.h>

int main() {
  int n;
  int i;
  int u;
  int used[100];
  int diag1[100];
  int diag2[100];
  int col[100];

  n = 11;
  u = 1;
  while (u <= n) {
    used[u] = 0;
    u = u+1;
  }

  u = 1;
  while (u <= 2 * n) {
    diag1[u] = diag2[u] = 0;
    u = u+1;
  }

  i = u = 1;
  while (i > 0) {
    while (i <= n && i != 0) {
      while (u <= n && (used[u] || diag1[u-i+n] || diag2[u+i]))
	      u = u + 1;
      if (u <= n) { // not used[u]; fill col[i] then try col[i+1]
	      col[i] = u;
	      diag1[u-i+n] = diag2[u+i] = used[u] = 1;
	      i = i+1; u = 1;
      } else {			// backtrack; try to find a new col[i-1]
	      i = i-1;
	      if (i > 0) {
	        u = col[i];
	        diag1[u-i+n] = diag2[u+i] = used[u] = 0;
	        u = u+1;
	      }
      }
    }

    if (i > n) {                // output solution, then backtrack
      int j;
      j = 1;
      while (j <= n) {
	      printf("%d\n", col[j]);
	      j = j+1;
      }
      printf("%d\n", -1);
      i = i-1;
      if (i > 0) {
	      u = col[i];
	      diag1[u-i+n] = diag2[u+i] = used[u] = 0;
	      u = u+1;
      }
    }
  }
}
