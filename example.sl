mut counter = 1 as i32;
let func = fn(a: i32) -> nil {
    if a == 7 {
        counter = counter * a;
    }else {
        counter = counter % a;
    }
};
