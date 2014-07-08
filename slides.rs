#![allow(dead_code)]
#![allow(unused_mut)]

use std::fmt::Show;

fn main() {
struct Dollars { amt: int }
struct Euros { amt: int }
trait Currency {
    fn render(&self) -> String;
    fn to_euros(&self) -> Euros;
}

impl Currency for Dollars {
    fn render(&self) -> String {
      format!("${}", self.amt)
    }
    fn to_euros(&self) -> Euros {
      let a = ((self.amt as f64) * 0.73);
      Euros { amt: a as int }
    }
}

impl Currency for Euros {
    fn render(&self) -> String {
      format!("€{}", self.amt)
    }
    fn to_euros(&self) -> Euros { *self }
}

fn add_as_euros<C:Currency>(a: &C, b: &C) -> Euros {
    let sum = a.to_euros().amt + b.to_euros().amt;
    Euros{ amt: sum }
}


}

#[cfg(off)]
fn main() {
    struct Pair<A,B> { a: A, b: B }
    fn add_b_twice<T>(p: Pair<int,T>,
                      f: fn (&T) -> int) -> int {
        match p {
            Pair{ a, b } => {
                a + f(&b) + f(&p.b)
            }
        }
    }
}

fn main_old3() {
    fn add3(x:int) -> int { x + 3 }
    enum E { A(fn (int) -> int), B(int) }
    let mut a = A(add3);
    let mut b = B(17);
    let p1 = &mut a;
    let p2 = &mut b;
    match p1 {
        &B(..) => fail!("cannot happen"),
        &A(ref adder) => {
            *p2 = B(0xdeadc0de);
            println!("{}", (*adder)(14));
        }
    }
}

fn main_old2() {
    let x: int = 3;
    let y: &int = &x;
    assert!(*y == 3);
    // assert!(y == 3); /* Does not type-check */

    struct Pair<A,B> { a: A, b: B }
    let p = Pair { a: 4, b: "hi" };
    let y: &int = &p.a;
    assert!(*y == 4);
}

fn main_old() {
    fn add3(x: int) -> int { x + 1 }
    let eight = twice(2, add3);
    println!("eight: {}", eight);
}

#[cfg(non_generic_twice)]
fn twice(x: int, f: fn (int) -> int) -> int {
    let w = f(x);
    println!("temp w: {}", w);
    let y = f(x);
    println!("temp y: {}", y);
    let z = f(y); return z;
}

#[cfg(not(non_generic_twice))]
fn twice<T:Copy+Show>(x: T, f: fn (T) -> T) -> T {
    let w = f(x);
    println!("temp w: {}", w);
    let y = f(x);
    println!("temp y: {}", y);
    let z = f(y); return z;
}

fn demo() {
    use std::mem::size_of;
    enum Lonely<A> { One(A), Two(A, A) }
    let size = size_of::<Lonely<(int,int,int,int,int)>>();
    let word_size = size_of::<int>();
    println!("words: {}", size / word_size);
}
