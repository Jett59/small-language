let greet = extern greet: (i32, bool) -> nil;
greet(16, false);
let fib = fn (n: i32) -> i32 {
    if n <= 1 {
        return n;
    }
    return fib (n - 1) + fib (n - 2);
};
