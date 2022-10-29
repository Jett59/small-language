let getInt = extern getInt: () -> i32;
let array = [1 as i32, 2, 3, 4, 5, 6] as [i32];
let exit = extern exit: (i32) -> nil;
exit(array[getInt() as u64]);
