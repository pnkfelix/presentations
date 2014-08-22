#![feature(macro_rules)]
#![allow(unused_variable)]
use std::fmt;

#[deriving(Clone,Show)] struct S { name: &'static str }

#[deriving(Clone,Show)] struct Df { name: &'static str }

#[deriving(Clone,Show)] struct Pair<X,Y>{ x: X, y: Y }


static mut current_indent: uint = 0;

fn indent() -> String {
    String::from_char(unsafe { current_indent }, ' ')
}

impl Drop for Df {
    fn drop(&mut self) {
        println!("{}dropping Df {}", indent(), self.name)
    }
}

fn foo(b: || -> bool) {  // DROP OBLIGATIONS
  let f3 = Df { name:"f3" };   // { f3 }
  let f4 = Df { name:"f4" };   // { f3,f4 }
  let f5 = Df { name:"f5" };   // { f3,f4,f5 }
  let p = Pair{ x: f4, 
                y: f5 }; // { f3,     , p }
  let _f10 = Df { name:"fA" }; // { f3,     , p,   _f10 }
  if b() {
                         // { f3,     , p,   _f10 }
    take_and_pass(p.x);
                         // { f3,     , p.y, _f10 }
  } else {
                         // { f3,     , p,   _f10 }
  }
  other_code();
}


fn take_and_pass<T:fmt::Show>(t: T) { unimplemented!() }
fn other_code() { unimplemented!() }


fn main() {
    println!("Running `foo(|| true)`");
    {
       foo(|| true);
    }
    println!("Running `foo(|| false)`");
    {
       foo(|| false);
    }
}
