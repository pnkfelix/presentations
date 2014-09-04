#lang racket
(require slideshow)
(require "rust-common.rkt")
(require "playpen.rkt")
(require "slide-rust-code.rkt")

(outline 'two)

(define (ocaml-above-rust ocaml-string rust-string)
  (vl-append
   (t "OCaml:") (frame (ocaml-tt/nl ocaml-string))
   (t "")
   (t "Rust:")  (frame (rust-tt/nl rust-string))))

(slide #:title "OCaml / Rust: basic syntax"
       (ocaml-above-rust
#<<OCAML
let y = let x = 2 + 3 in x > 5 in
if y then x + 6 else x + 7
OCAML
#<<RUST
let y = { let x = 2 + 3; x > 5 };
if y { x + 6 } else { x + 7 }
RUST
#;
#<<FAKE_CLOSE_CURLY
} (this whole HERE string is actually all commented out)
FAKE_CLOSE_CURLY
))

(slide #:title "OCaml / Rust: functions"
       (ocaml-above-rust
#<<OCAML
let add3 x = x + 3 in
let y = add3 7 > 5 in
...                            
OCAML
#<<RUST
fn add3(x:int) -> int { x + 3 }
let y = add3(3) > 5;
RUST
))

(call-with-url-and-code
#<<RUST
fn main() {
    // SLIDE BEGIN
    fn add3_left<A>((x,y):(int, A)) -> (int, A) {
        (x + 3, y)
    }
    let y = add3_left((7,"hi")) > (10,"lo");
    // SLIDE FINIS
    println!("y: {}", y);
}
RUST
(lambda (url code)
(let ((ocaml-string
#<<OCAML
let add3_left (x, y) = (x + 3, y) in
let y = add3_left (7,"hi") > (10,"lo") in
...                                          
OCAML
))
  (slide #:title "OCaml / Rust: (irrefutable) pattern binding"
         (vl-append
          (t "OCaml:") (frame (ocaml-tt/nl ocaml-string))
          (t "Rust:")  (frame code)
          url)
         'next
         (item "(A generic type parameter snuck in above)")
         ))))

(call-with-url-and-code
#<<RUST
fn main() {
    // SLIDE BEGIN
    enum Lonely<A> { One(A), Two(A, A) }
    fn combined(l: Lonely<int>) -> int {
        match l {
            One(a)    => a,
            Two(a, b) => a + b,
        }
    }                                          
    // SLIDE FINIS
    let bad_as_one = Two(3, 7);
    println!("combined: {}", combined(bad_as_one));
}
RUST
(lambda (url code)
(slide #:title "OCaml / Rust: pattern matching"
       (let ((ocaml-string
#<<OCAML
type 'a lonely = One of 'a | Two of 'a * 'a;;
let combined l =
    match l with
      One a      -> a
    | Two (a, b) -> a + b
in ...
OCAML
))
         (vl-append
   (t "OCaml:") (frame (ocaml-tt/nl ocaml-string))
   (t "Rust:") (frame code)
   url
)))))

(slide-code/url "Rust: Bounded Polymorphism (No functors)"
                #<<RUST
enum Lonely<A> { One(A), Two(A, A) }
trait Plus { fn plus(self, rhs: Self) -> Self; }
fn combined<N:Plus>(l: Lonely<N>) -> N {
    match l {
        One(a)    => a,
        Two(a, b) => a.plus(b),
    }
}
// SLIDE FINIS
impl Plus for int {
    fn plus(self, rhs: int) -> int {
        self + rhs
    }
}

fn main() {
    let bad_as_one = Two(3i, 7i);
    println!("combined: {}", combined(bad_as_one));
}
RUST
)

(define ex-currency-traits-def
  #<<RUST_TRAITS_DEF
// "struct" is Rust's record syntax.
struct Dollars { amt: int }
struct Euros { amt: int }
trait Currency {
    fn render(&self) -> String;
    fn to_euros(&self) -> Euros;
}
RUST_TRAITS_DEF
)

(define ex-currency-impl-def
  #<<RUST_IMPL_DEF
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
RUST_IMPL_DEF
)

(define add-as-euros-def
  #<<RUST_DEF
fn add_as_euros<C:Currency>(a: &C, b: &C) -> Euros {
    let sum = a.to_euros().amt + b.to_euros().amt;
    Euros{ amt: sum }
}
RUST_DEF
)

(define add-eu-eu-use
  #<<RUST_DEF
    let eu100 = Euros { amt: 100 };
    let eu200 = Euros { amt: 200 };
    println!("{:?}", add_as_euros(&eu100, &eu200));
RUST_DEF
)

(define add-us-us-use
  #<<RUST_DEF
    let us100 = Dollars { amt: 100 };
    let us200 = Dollars { amt: 200 };
    println!("{:?}", add_as_euros(&us100, &us200));
RUST_DEF
)

(define add-us-eu-use-breaks
  #<<RUST_DEF
    let us100 = Dollars { amt: 100 };
    let eu200 = Euros { amt: 200 };
    println!("{:?}", add_as_euros(&us100, &eu200));
RUST_DEF
)

(define add-us-eu-use-error-msg
  #<<RUSTC_ERR
error: mismatched types: expected `&Dollars`
       but found `&Euros` (expected struct Dollars
       but found struct Euros)
     println!("{:?}", add_as_euros(&us100, &eu200));
                                           ^~~~~~
RUSTC_ERR
)

#;
(slide-code/url "Rust: Bounded Polymorphism (No functors)"
                ;; (string-append ex-currency-traits-def add-as-euros-def)
#<<RUST
#[deriving(Show)] // SLIDE HIDE
struct Dollars { amt: int }
// SLIDE START
// "struct" is Rust's record syntax.
#[deriving(Show)] // SLIDE HIDE
struct Euros { amt: int }
#[deriving(Show)] // SLIDE HIDE
struct SEK { amt: int }
trait Currency {
    fn render(&self) -> String;
    fn to_euros(&self) -> Euros;
}

fn add_vals_euros<C:Currency>(a: C, b: C) -> Euros {
    let sum = a.to_euros().amt + b.to_euros().amt;
    Euros{ amt: sum }
}
// SLIDE FINIS

// (the function above is silly; it may look more generic than it really is.)

fn main() {
    let benjamin = Dollars { amt: 100 };
    let baroque_rococo = Euros { amt: 100 };
    let linnaeus = SEK { amt: 100 };
    println!("two benjamin: {}", add_vals_euros(benjamin, benjamin).render());
    println!("two linnaeus: {}", add_vals_euros(linnaeus, linnaeus).render());
}

// (the rest of this is just the necessary impls to identify
//  Dollars/Euros as Currency.)

impl Currency for Dollars {
    fn render(&self) -> String {
      format!("${}", self.amt)
    }
    fn to_euros(&self) -> Euros {
      let a = 0.76 * (self.amt as f64);
      Euros { amt: a as int }
    }
}

impl Currency for SEK {
    fn render(&self) -> String {
      format!("{} SEK", self.amt)
    }
    fn to_euros(&self) -> Euros {
      let a = 0.1086 * (self.amt as f64);
      Euros { amt: a as int }
    }
}

impl Currency for Euros {
    fn render(&self) -> String {
      format!("€{}", self.amt)
    }
    fn to_euros(&self) -> Euros { *self }
}
RUST
    )
#;
(slide #:title "Rust: Bounded Polymorphism (No functors)"
       (vl-append
        (rust-tt/nl ex-currency-traits-def)
        (rust-tt/nl add-as-euros-def)))

#;
(slide #:title "Trait Impls"
       (rust-tt/nl ex-currency-impl-def))

#;
(slide #:title "Static Resolution (1)"
       (rust-tt/nl add-as-euros-def)
       'next
        (rust-tt/nl add-eu-eu-use)
        'next
        (hbl-append (t " ⇒ ") (tt "Euros{amt: 300}")))

#;
(slide #:title "Static Resolution (2)"
       (rust-tt/nl add-as-euros-def)
       (rust-tt/nl add-us-us-use)
       'next
       (hbl-append (t " ⇒ ") (tt "Euros{amt: 219}")))

#;
(let ((bot (hbl-append (t " ⇒ ") (tt "Euros{amt: 219}"))))
  (slide #:title "Static Resolution (!)"
         (rust-tt/nl add-as-euros-def)
         'next
         (rust-tt/nl add-us-eu-use-breaks)
         'next
         'alts
         (list
          (list (hbl-append (t " ⇒ ") (ghost (tt "Euros{amt: 219}"))))
          (list (pin-over (ghost bot)
                          (- (pict-width bot)) 0
                          (tt/nl add-us-eu-use-error-msg))))))

#;
(slide #:title "Dynamic Dispatch"
       (rust-tt/nl #<<RUST_DISPATCH
fn add_as_euros<C:Currency>(a: &C, b: &C) -> Euros {
    let sum = a.to_euros().amt + b.to_euros().amt;
    Euros{ amt: sum }
}

fn accumeuros(a: &Currency, b: &Currency) -> Euros {
    let sum = a.to_euros().amt + b.to_euros().amt;
    Euros{ amt: sum }
}

let us100 = Dollars { amt: 100 };
let eu200 = Euros { amt: 200 };
println!("{:?}", accumeuros(&us100 as &Currency,
                            &eu200 as &Currency));
RUST_DISPATCH

)
       'next
       (hbl-append (t " ⇒ ") (tt "Euros{amt: 273}")))
