#![feature(specialization)]
#![feature(catch_expr)]
#![feature(proc_macro)]

#[macro_use]
extern crate weight_derive;
use weight_derive::*;

pub mod a00;
pub mod c00_nikos_ghost;
pub mod d00_interlude;
pub mod e00_epochs_past;
pub mod g00_epochs_present;
pub mod i00_epochs_future;
pub mod l00_end_of_it;
pub mod z00;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}

fn main() {
    println!("It works");
    g00_epochs_present::main();
    // let result = l00_end_of_it::f();
    // println!("f: {:?}", result);
}
