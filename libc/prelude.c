#include <stdio.h>

int print(int c) {
  printf("%d", c);
  return 0;
}

int println(int c) {
  printf("%d\n", c);
  return 0;
}

int print_int(int c) { return println(c); }