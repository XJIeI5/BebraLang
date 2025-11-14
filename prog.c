#include <stdint.h>
#include <stdio.h>
int32_t sum(int32_t a, int32_t b) {
    return (a+b);
}
uint64_t fib(uint64_t n) {
    uint64_t c = 0;
    uint64_t a = 0;
    uint64_t b = 1;
    for (uint64_t i = 0; (i<n); (i++)) {
    c = b;
    b = (a+b);
    a = c;
}
    return a;
}
void main() {
    printf("Fibonacci series!\n");
    printf("N:\t");
    for (uint64_t i = 0; (i<=10); (i++)) {
    printf("%llu\t", i);
}
    printf("\n");
    printf("V:\t");
    for (uint64_t i = 0; (i<=10); (i++)) {
    printf("%llu\t", fib(i));
}
    printf("\n");
}
