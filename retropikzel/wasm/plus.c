#include <stdio.h>
#include <emscripten/emscripten.h>
#include <stdint.h>

#ifdef __cplusplus
#define EXTERN extern "C"
#else
#define EXTERN
#endif

EXTERN EMSCRIPTEN_KEEPALIVE uint64_t x = 100;

EXTERN EMSCRIPTEN_KEEPALIVE int plus(int a, int b) {
    return a + b;
}

/*
int plus_three(int a, int b, int c) {
    return a + b + c;
}
*/
