let fib = fn (a: i32) -> i32 {
    if a <= 1 {
        return a;
    }
    return fib(a - 1) + fib(a - 2);
};
let exit = extern exit: (i32) -> nil;
let result = fib(17);
exit(result);
