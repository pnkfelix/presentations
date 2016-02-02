```rust
fn twice_i32<F>(x0: i32, f: &F) -> i32 where F: Fn(i32) -> i32 {
    //      ~~~          ~~~~~               ~~~~~~~~~~~~~~~~~
    //       |             |                         |
    // Type Parameter      |                         |
    //                     |                         |
    //     `f` is reference to an `F`                |
    //                                               |
    //         `F` is any function that consumes an `i32` and produces an `i32`

    let x1 = f(x0); // apply `f` once ...

    f(x1)           // ... and again
}


fn demo1() {
    fn add_1(x: i32) -> i32 { x + 1 }
    println!("demo1 twice_i32(0, add_1): {}", twice_i32(0, &add_1));
    println!("demo1 twice_i32(0, λz.z+1): {}", twice_i32(0, &|y| y+1));
    let w = 3;
    println!("demo1 twice_i32(0, add_w): {}", twice_i32(0, &|z| z+w));
}

pub fn twice<X, F>(x: X, f: &F) -> X where F: Fn(X) -> X {
    let x1 = f(x);
    f(x1)
}

fn demo2() {
    println!("twice(0, add_1): {}", twice(0i32, &|y| y+1));

    // println!("twice_peano(0): {}", twice_peano(0i32)); // XXX (see exercise below)
}

pub fn main() {
    demo1();
    demo2();
    demo3();
}

fn demo3() {
    let mut state = 0;

    {
        let f1 = Funnel { x: &mut state, f: |s| { *s = *s + 1; s } };
        f1.go();
    } // Q for audience: what happens if we remove this pair of curly braces? Why?

    println!("demo2 state: {}", state);
}

struct Funnel<X, F> where F: FnOnce(X) -> X {
    x: X,
    f: F,
}

impl<X, F> Funnel<X, F> where F: FnOnce(X) -> X {
    fn go(self) -> X {
        let Funnel { x: object, f: callback } = self;
        return callback(object);
    }

    // fn go_twice(self) -> X {
    //     let Funnel { x: object, f: callback } = self;
    //     return callback(callback(object));
    // }
}
```
