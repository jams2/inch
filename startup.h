#include <stdint.h>

typedef uint64_t ptr;

#define fxshift 2
#define fxmask 0x03
#define fxtag 0x00

#define chrmask 0x3F
#define chrshift 8
#define chrtag 0x0F

#define bool_f 0x2F
#define bool_t 0x6F
#define null 0x3F

#define pairtag 1
#define objmask 7

#define char_tab 9
#define char_newline 10
#define char_vt 11
#define char_ff 12
#define char_return 13
#define char_space 32

#define is_fixnum(x) ((x & fxmask) == fxtag)
#define is_char(x) ((x & chrmask) == chrtag)
#define is_pair(x) ((x & objmask) == pairtag)

#define car(pair) (*(ptr *)(pair - 1))
#define cdr(pair) (*(ptr *)(pair + 7))

typedef struct {
  uint64_t rax; // 0 Scratch
  uint64_t rbx; // 8 Preserved
  uint64_t rcx; // 16 Scratch
  uint64_t rdx; // 24 Scratch
  uint64_t rsi; // 32 Scratch
  uint64_t rdi; // 40 Scratch
  uint64_t rbp; // 48 Preserved
  uint64_t rsp; // 56 Stack Pointer (Preserved)
  uint64_t r8;  // 64 Scratch
  uint64_t r9;  // 72 Scratch
  uint64_t r10; // 80 Scratch
  uint64_t r11; // 88 Scratch
  uint64_t r12; // 96 Preserved
  uint64_t r13; // 104 Preserved
  uint64_t r14; // 112 Preserved
  uint64_t r15; // 120 Preserved
} Context;

extern ptr scheme_entry(Context *, char *, char *);
