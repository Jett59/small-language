let func = fn (n: i32) -> i32 {
    if n == 0 {
        return 0;
    }else if n == 1 {
        return 1;
    }else {
        return func(n - 1) + func(n - 2);
    }
};
let exit = extern exit: (i32)-> nil;
let getInt = extern getInt: () -> i32;
exit(func(getInt()));
