// Simple math functions for WASM testing
// Compile with:
//   clang --target=wasm32 -O3 -nostdlib -Wl,--no-entry \
//         -Wl,--export=add -Wl,--export=multiply -Wl,--export=fibonacci \
//         math.c -o math.wasm

long add(long a, long b) {
    return a + b;
}

long multiply(long a, long b) {
    return a * b;
}

long fibonacci(long n) {
    if (n <= 1) {
        return n;
    }
    return fibonacci(n - 1) + fibonacci(n - 2);
}

long factorial(long n) {
    if (n <= 1) {
        return 1;
    }
    return n * factorial(n - 1);
}
