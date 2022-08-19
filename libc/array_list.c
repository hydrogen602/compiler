#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "mem.h"

struct ArrayList {
  int32_t size;
  int32_t capacity;
  int32_t *data; // FIXME: factor out type here, somehow?
};

typedef struct ArrayList *ArrayList_t;

ArrayList_t ArrayList() {
  ArrayList_t ptr = (ArrayList_t)malloc_rc(sizeof(struct ArrayList));
  ptr->size = 0;
  ptr->capacity = 0;
  ptr->data = NULL;
  return ptr;
}

void ArrayList_destroy(ArrayList_t arr) {
  if (rc_decr((ref_count_ptr)arr) == NO_LONGER_IN_USE) {
    arr->size = 0;
    arr->capacity = 0;
    if (arr->data != NULL) {
      free(arr->data);
    }
    arr->data = NULL;
    free_rc((ref_count_ptr)arr);
  }
}

void ArrayList_append(ArrayList_t arr, int32_t val) {
  if (arr->size >= arr->capacity) {
    if (arr->capacity == 0) {
      arr->capacity = 8;
    } else {
      arr->capacity *= 2;
    }
    arr->data = realloc(arr->data, arr->capacity);
    if (arr->data == NULL) {
      fprintf(stderr, "Memory allocation failed");
      exit(EXIT_FAILURE);
    }
  }
  arr->data[arr->size] = val;
  arr->size++;
}

int32_t ArrayList_pop(ArrayList_t arr) {
  if (arr->size == 0) {
    fprintf(stderr, "Nothing to pop");
    exit(EXIT_FAILURE);
  }
  arr->size--;
  return arr->data[arr->size];
}

int32_t ArrayList_len(ArrayList_t arr) { return arr->size; }
