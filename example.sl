let fib = fn (a: i32) -> i32 {
    if a == 0 {
        return 0;
        }else if a == 1 {
            return 1;
        }else {
        return fib(a - 1) + fib(a - 2);
    }
};
let exit = extern exit: (i32)-> nil;
exit(fib(17));
