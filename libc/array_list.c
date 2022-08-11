#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct {
  int32_t size;
  int32_t capacity;
  int32_t *data;
} ArrayList_t;

ArrayList_t newArrayList() {
  ArrayList_t arr = {0, 0, NULL};
  return arr;
}

void append(ArrayList_t arr, int32_t val) {
  // currently broken
  if (arr.size >= arr.capacity) {
    if (arr.capacity == 0) {
      arr.capacity = 8;
    } else {
      arr.capacity *= 2;
    }
    arr.data = realloc(arr.data, arr.capacity);
    if (arr.data == NULL) {
      fprintf(stderr, "Memory allocation failed");
      exit(EXIT_FAILURE);
    }
  }
  arr.data[arr.size] = val;
  arr.size++;
}

int32_t pop(ArrayList_t arr) {
  if (arr.size == 0) {
    fprintf(stderr, "Nothing to pop");
    exit(EXIT_FAILURE);
  }
  arr.size--;
  return arr.data[arr.size];
}

int32_t len(ArrayList_t arr) { return arr.size; }
