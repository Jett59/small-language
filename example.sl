let fib = fn (a: i32) -> i32 {
    if a <= 1 {
        return a;
    }
    return fib(a - 1) + fib(a - 2);
};