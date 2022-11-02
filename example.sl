let exit = extern exit: (i32)-> nil;
let getInt = extern getInt: ()-> i32;
let x = getInt();
let y = getInt();
exit(x * y + x / y - (x + y) / x + x);

