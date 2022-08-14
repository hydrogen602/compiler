#include <stdio.h>

int fibR(int n) {
  if (n == 0) {
    return 0;
  } else if (n == 1) {
    return 1;
  } else {
    return fibR(n - 1) + fibR(n - 2);
  }
}

int main() {
  for (int i = 30; i < 40; ++i) {
    printf("%d\n", i);
    printf("%d\n", fibR(i));
  }
}