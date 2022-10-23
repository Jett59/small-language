mut glob = 72 as i32;
let fn1 = fn (a: i32) -> nil {
    glob = glob + a;
};
let fn2 = fn (a: i32) -> nil {
    glob = glob - a;
};
mut anFn = fn1;

let switch = fn (a: i32) -> nil {
    if anFn == fn1 {
        anFn = fn2;
    }else {
        anFn = switch;
    }
};
