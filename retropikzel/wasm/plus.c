#include <stdio.h>
#include <emscripten/emscripten.h>
#include <stdint.h>

#ifdef __cplusplus
#define EXTERN extern "C"
#else
#define EXTERN
#endif

EXTERN EMSCRIPTEN_KEEPALIVE int x = 1;
EXTERN EMSCRIPTEN_KEEPALIVE int y = 50000;

EXTERN EMSCRIPTEN_KEEPALIVE int plus(int a, int b) {
    return a + b + x;
}

/*
int plus_three(int a, int b, int c) {
    return a + b + c;
}
*/
