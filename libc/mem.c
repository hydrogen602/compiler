#include "mem.h"
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

// There probably a more reliable way to do this, but for now I'm just checking
// pointer size
// I need the word size to keep memory aligned
#define WORD sizeof(WORD_t)
#define WORD_t char * // is this a complete misuse of pointers? probably

/**
 * @brief Terminate the program when memory cannot be allocated, i.e. malloc
 * returns NULL
 *
 */
void mem_alloc_error() {
  fprintf(stderr, "Memory allocation error\n");
  exit(EXIT_FAILURE);
}

/**
 * @brief Increments the reference counter
 *
 * @param ptr points to the data (not to the ref count)
 */
void rc_incr(ref_count_ptr ptr) {
#ifdef SANITY_CHECK
  if (ptr == NULL) {
    fprintf(stderr, "rc_incr called on a null pointer\n");
    exit(EXIT_FAILURE);
  }
#endif
  WORD_t *word_ptr = (WORD_t *)ptr;
  ++(*(word_ptr - 1));
}

/**
 * @brief Decrements the reference counter
 *
 * @param ptr points to the data (not to the ref count)
 * @return rc_status_t whether the reference should be cleaned up or not.
 */
rc_status_t rc_decr(ref_count_ptr ptr) {
#ifdef SANITY_CHECK
  if (ptr == NULL) {
    fprintf(stderr, "rc_decr called on a null pointer\n");
    exit(EXIT_FAILURE);
  }
#endif
  WORD_t *word_ptr = (WORD_t *)ptr;
#ifdef SANITY_CHECK
  if (*(word_ptr - 1) <= (WORD_t)0) {
    fprintf(
        stderr,
        "\x1b[33mWarning: rc_decr called on a pointer that should already be "
        "freed: ptr = %p\x1b[0m\n",
        (void *)(word_ptr - 1));
  }
#endif
  // if the count is 0 (or less), it is no longer in use.
  // however, we can't clean it up right now since
  // a custom destructor may have to be called
  return (--(*(word_ptr - 1)) <= (WORD_t)NULL) ? NO_LONGER_IN_USE : IN_USE;
}

/**
 * @brief Get the number of references
 *
 * @param ptr points to the data (not to the ref count)
 * @return size_t the reference count
 */
size_t rc_count(ref_count_ptr ptr) {
#ifdef SANITY_CHECK
  if (ptr == NULL) {
    fprintf(stderr, "rc_count called on a null pointer\n");
    exit(EXIT_FAILURE);
  }
#endif
  WORD_t *word_ptr = (WORD_t *)ptr;
  return (size_t)(*(word_ptr - 1));
}

/**
 * @brief malloc's a chunk of memory of given size and adds a reference pointer
 *
 * @param __size number of bytes to allocate (ref counter is added to this)
 * @return ref_count_ptr is not null
 */
ref_count_ptr malloc_rc(size_t __size) {
  size_t final_size = __size + WORD;

  ref_count_ptr ptr = (ref_count_ptr)malloc(final_size);
  if (ptr == NULL) {
    mem_alloc_error();
  }
#ifdef MDEBUG
  fprintf(stderr, "Allocated block of size %lu + %lu, ptr = %p\n", __size, WORD,
          (void *)ptr);
#endif
  memset(ptr, 0, WORD);
  ptr += WORD;
  rc_incr(ptr);
  return ptr;
}

/**
 * @brief free a reference counted value
 *
 * @param ptr points to the data (not to the ref count)
 */
void free_rc(ref_count_ptr ptr) {
#ifdef SANITY_CHECK
  if (rc_count(ptr) > 0) {
    fprintf(stderr,
            "Warning: Freeing pointer with a reference count greater than "
            "zero!\nptr = %p\n",
            (void *)ptr);
  }
#endif
  ptr -= WORD;

#ifdef MDEBUG
  fprintf(stderr, "Deallocated block: ptr = %p\n", (void *)ptr);
#endif
  free(ptr);
}
