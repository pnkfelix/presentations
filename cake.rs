// Copyright 2014 Felix S. Klock II. See the COPYRIGHT
// file at the top-level directory of this distribution.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

enum Flavor { chocolate, vanilla }
struct Cake { flavor: Flavor, num_slices: int }
impl Cake {
    fn eat_slice(&mut self) {
        self.num_slices -= 1;
    }
}

fn eat_at_least(cake: &mut Cake, threshold: &int) {
    let mut eaten_so_far = 0;
    while (cake.num_slices > 0 && eaten_so_far < *threshold) {
        cake.eat_slice();
        eaten_so_far += 1;
    }
}

#[cfg(buggy)]
fn eat_entire(cake: &mut Cake) {
    eat_at_least(cake, &cake.num_slices);
}

#[cfg(fixed)]
fn eat_entire(cake: &mut Cake) {
    let n = cake.num_slices;
    eat_at_least(cake, &n);
}

fn main () {
    let mut cake = birthday_cake(vanilla, 16);
    status(&cake, "at outset");
    eat_at_least(&mut cake, &2);
    status(&cake, "after 2");
    eat_entire(&mut cake);
    status(&cake, "finally");
}

fn status(cake: &Cake, when: &str) {
    println!("cake {} has {} slices.", when, cake.num_slices);
}

fn birthday_cake(f:Flavor, num_slices:int) -> Cake {
    Cake { flavor: f, num_slices: num_slices }
}
