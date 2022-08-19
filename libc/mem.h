#ifndef MEM_H_
#define MEM_H_

#include <stdlib.h>

typedef char *ref_count_ptr;

typedef enum { IN_USE, NO_LONGER_IN_USE } rc_status_t;

void mem_alloc_error();

void rc_incr(ref_count_ptr ptr);

rc_status_t rc_decr(ref_count_ptr ptr);

size_t rc_count(ref_count_ptr ptr);

ref_count_ptr malloc_rc(size_t __size);

void free_rc(ref_count_ptr ptr);

#endif
