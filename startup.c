#include "startup.h"
#include <stdio.h>
#include <sys/mman.h>
#include <unistd.h>

void print_ptr_rec(ptr x) {
  if (is_fixnum(x)) {
    printf("%ld", ((int64_t)x) >> fxshift);
  } else if (x == bool_f) {
    printf("#f");
  } else if (x == bool_t) {
    printf("#t");
  } else if (x == null) {
    printf("()");
  } else if (is_char(x)) {
    int c = x >> chrshift;
    if (c == char_tab) {
      printf("#\\tab");
    } else if (c == char_return) {
      printf("#\\return");
    } else if (c == char_newline) {
      printf("#\\newline");
    } else if (c == char_ff) {
      printf("#\\ff");
    } else if (c == char_vt) {
      printf("#\\vt");
    } else if (c == char_space) {
      printf("#\\space");
    } else {
      printf("#\\%c", c);
    }
  } else if (is_pair(x)) {
    ptr current = x;
    ptr a = car(x);
    ptr d = cdr(x);

    printf("(");
    for (;;) {
      print_ptr_rec(a);
      if (is_pair(d)) {
	printf(" ");
	current = d;
	a = car(current);
	d = cdr(current);
      } else if (d == null) {
	break;
      } else {
	printf(" . ");
	print_ptr_rec(d);
	break;
      }
    }
    printf(")");
  }
}

void print_ptr(ptr x) {
  print_ptr_rec(x);
  printf("\n");
}

static char *allocate_protected_space(int size) {
  int page = getpagesize();
  int status;
  int aligned_size = ((size + page - 1) / page) * page;
  char *p = mmap(0, aligned_size + 2 * page, PROT_READ | PROT_WRITE,
                 MAP_ANON | MAP_PRIVATE, -1, 0);
  if (p == MAP_FAILED) {
    fprintf(stderr, "mmap() failed\n");
    return (char *)-1;
  }
  status = mprotect(p, page, PROT_NONE);
  if (status != 0) {
    fprintf(stderr, "mprotect() failed\n");
    return (char *)-1;
  }
  status = mprotect(p + page + aligned_size, page, PROT_NONE);
  if (status != 0) {
    fprintf(stderr, "mprotect() failed\n");
    return (char *)-1;
  }
  return p + page;
}

static void deallocate_protected_space(char *p, int size) {
  int page = getpagesize();
  int status;
  int aligned_size = ((size + page - 1) / page) * page;
  status = munmap(p - page, aligned_size + 2 * page);
  if (status != 0) {
    fprintf(stderr, "munmap() failed\n");
  }
}

int main() {
  Context ctx;
  int stack_size = 16 * 4096; /* Holds 16K cells */
  char *stack_top = allocate_protected_space(stack_size);
  char *stack_base = stack_top + stack_size;
  char *heap = allocate_protected_space(stack_size);
  print_ptr(scheme_entry(&ctx, stack_base, heap));
  deallocate_protected_space(stack_top, stack_size);
  deallocate_protected_space(heap, stack_size);
  return 0;
}
