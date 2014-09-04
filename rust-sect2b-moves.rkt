#lang racket
(require slideshow)
(require "slides-common.rkt")
(require "rust-common.rkt")
(require unstable/gui/slideshow)
(require "playpen.rkt")
(require "slide-rust-code.rkt")

(define (adjust-find-recentering some-find pict)
  (lambda (p pp)
    (let ((w (pict-width pict))
          (h (pict-height pict)))
      (let-values (((dx dy) (some-find p pp)))
        (values (- dx (/ w 4)) ; Nope, I do not understand why 2 does not work but 4 does here.
                (- dy (/ h 4)))))))

(define W 40)
(define (box-of val) (cc-superimpose (rectangle W W) (text val)))

(define (make-five-tup)
   (define tup-a.val0 (box-of "1"))
   (define tup-a.val1 (box-of "2"))
   (define tup-a.val2 (box-of "3"))
   (define tup-a.val3 (box-of "4"))
   (define tup-a.val4 (box-of "5"))
   (hc-append 0 tup-a.val0 tup-a.val1 tup-a.val2 tup-a.val3 tup-a.val4))

(define rendering-of-ocaml-lonely-tuples
 (let ()
   (define two.tag  (box-of "(Two)"))
   (define (box) (rectangle W W))
   (define two.val0 (box))
   (define two.val1 (box))
   (define pict-two (hc-append 0 two.tag two.val0 two.val1))
   (define tup-a (make-five-tup))
   (define tup-b (make-five-tup))
   (define dot (disk 3))
   (define (add-dot base-pict where-pict)
     (define dot (disk 3))
     (pin-over base-pict where-pict (adjust-find-recentering cc-find dot) dot))
   (define left (add-dot (add-dot pict-two two.val0) two.val1))
   (let* ((combined
           (let ((c 3)
                 (r 4))
             (table c
                    (list left (blank W) (blank W)
                          (blank W) (blank W) tup-b
                          (blank W) (blank W) (blank W)
                          (blank W) (blank W) tup-a)
                    (make-list c cc-superimpose)
                    (make-list r cc-superimpose)
                    (make-list (- c 1) 0)
                    (make-list (- r 1) 0)
                    )))
   
          (combined 
           (pin-arrow-line 10 combined
                           #:start-angle (- (/ pi 2))
                           #:end-angle (/ pi -8)
                           two.val0 cc-find
                           tup-a lc-find))
          (combined
           (pin-arrow-line 10 combined
                           #:start-angle (- (/ pi 2))
                           #:end-angle (/ pi -8)
                           two.val1 cc-find
                           tup-b lc-find)))
     combined
     )))

(define rendering-of-rust-lonely-tuples
  (let ()
    (define two.tag  (box-of "(Two)"))
    (define tup-a (make-five-tup))
    (define tup-b (make-five-tup))
    (define pict-two (hc-append 0 two.tag tup-a tup-b))
    pict-two))

(define rendering-of-rust-lonely-tuple
  (let ()
    (define one.tag  (box-of "(One)"))
    (define tup (make-five-tup))
    (define (box) (rectangle W W))
    (define pict-one (hc-append 0 one.tag tup (box) (box) (box) (box) (box)))
    pict-one))

(slide
 #:title "OCaml / Rust: value model (move semantics)"
 (item "In OCaml, under the hood, large values are (tagged) references.")
 (item "Passing one parameter == copy one word")
 (subitem "(a word-sized literal, or a tagged pointer to block on heap)")
 (item "Things are different in Rust.")
 )

(slide
 #:title "A mini-puzzle"
 (item "What does this print?")
 (vl-append
  (t "OCaml:") 
  (ocaml-tt/nl #<<OCAML
# type 'a lonely = One of 'a | Two of 'a * 'a;;
# Obj.size(Obj.repr(1,2,3,4,5));;
- : int = 5
# Obj.size(Obj.repr(Two((1,2,3,4,5),
                        (1,2,3,4,5))));;
OCAML
))
 (item (t "Answer:"))
 'next
 (para
  #:align 'left
  (ocaml-tt/nl #<<OCAML
- : int = 2
OCAML
)))

(slide
 #:title "Why only 2 words?"
 (vl-append
  (ocaml-tt/nl #<<OCAML
# type 'a lonely = One of 'a | Two of 'a * 'a
# Obj.size(Obj.repr(1,2,3,4,5));;
- : int = 5
# Obj.size(Obj.repr(Two((1,2,3,4,5),
                        (1,2,3,4,5))));;
- : int = 2
OCAML
    ))
 (item "Here is how OCaml represents a " (tt "Two"))
  rendering-of-ocaml-lonely-tuples
  )

(call-with-url-and-code
#<<RUST
fn main() {
    // SLIDE BEGIN
    use std::mem::size_of;
    enum Lonely<A> { One(A), Two(A, A) }
    let size =
        size_of::<Lonely<(int,int,int,int,int)>>();
    let word_size = size_of::<int>();
    println!("words: {}", size / word_size);
    // SLIDE FINIS
}
RUST
 (lambda (url code)
  (slide
   #:title "The same puzzle in Rust"
   url
   (frame code)
   'next
   (item "Prints " (tt "words: 11"))
   (item "Here is how Rust represents a " (tt "Two"))
   rendering-of-rust-lonely-tuples
   'next
   (item "Here is how Rust represents a " (tt "One"))
   rendering-of-rust-lonely-tuple
   )))

(slide #:name "Implications of move semantics"
       'alts
       (list (list (big-t "Implications"))))

(call-with-url-and-code
 #<<RUST
use std::fmt::Show;
// SLIDE BEGIN
fn twice<T:Show>(x: T, f: fn (T) -> T) -> T {
    let w = f(x);
    println!("temp w: {}", w);
    let y = f(x);
    println!("temp y: {}", y);
    let z = f(y); return z;
}
// SLIDE FINIS
fn main() {
  fn times3(x:i32) -> i32 { x * 3 }
  let answer = twice(2, times3);
  println!("answer: {}", answer);
}
RUST
(lambda (url code)
  (slide
   #:title "To Move or To Copy?"
   (para "This does not compile (" url ")")
   (frame code)
   'next
   (rust-tt/nl #<<RUST
error: use of moved value: `x`
let y = f(x);
          ^
note: `x` moved here because it has non-copyable
      type `T` (perhaps use clone()?)
let w = f(x);
          ^
RUST
))))

(define rust-snippet
#<<RUST
let y = { let x = 2 + 3; x > 5 };
if y { x + 6 } else { x + 7 }
RUST
)

(slide #:title "What sleight of hand is this?"
       (item "\"You said 'move semantics'; looks like linear or affine types.\"")
       (item "Did we not see a bunch of examples earlier like:"
             (rust-tt/nl rust-snippet))
       'next
       (item "Obviously" (rust-tt "x") "is not being used linearly" (bold "there"))
       (item "Magic?"))

(slide #:title "Clarke's third law"
       'alts
       (list (list (item "It's not magic; it's the type system"))
             (list (item "It's not magic;" (strike (t "it's the type system"))
                         "it's the type + trait system")
                   'next
                   (item "The" (tt "Copy") "bound expresses that a type is freely copyable")
                   (subitem "and it is checked by the compiler")
                   'next
                   (item "Many built-in types implement" (tt "Copy") "...")
                   (item "... but a type parameter with no given bounds does not."))))

(call-with-url-and-code
  #<<RUST
use std::fmt::Show;
// SLIDE BEGIN
fn twice<T:Show+Copy>(x: T, f: fn (T) -> T) -> T {
            // ^~~~~ new code here // COLOR:red
    let w = f(x);
    println!("temp w: {}", w);
    let y = f(x);
    println!("temp y: {}", y);
    let z = f(y); return z;
}
// SLIDE FINIS
fn main() {
  fn times3(x:i32) -> i32 { x * 3 }
  let answer = twice(2, times3);
  println!("answer: {}", answer);
}
RUST
(lambda (url code)
  (slide
   #:title "To Move or To Copy? (II)"
   (para "This version works (" url ")")
   (frame code)
   'next
   (para "but that's not the point.")
   (item "(Cannot generally just add" (tt "Copy") "bounds)"))))

(slide #:name "Why all the fuss about move semantics?"
       'alts
       (list (list (big-t "Why all the fuss about move semantics?"))))

(outline 'three)

(slide
 #:title "Rust: Values and References"
 (para "Life outside of ref-cells")
 (para "There are three core types" (tt "T") "to think about.")
 (item (tt "     T") "  non-reference")
 (comment "e.g. ints, tuples, struct instances, ...")
 (item (tt "    &T") "  shared reference")
 (item (rust-tt "&mut T") "  mutable unaliased reference")
 'next
 (para "okay there is" (tt "*T") "  too, aka " (rust-tt "unsafe") "pointers")
 'next
 (subitem "(and library smart pointers like" (tt "Box<T>") "or"
  (tt "Rc<T>") ", but those are not" (bold "core") ")")
 )

(slide-code/url "&T : shared reference"
 #<<RUST
fn main() {
    // SLIDE BEGIN
    let x: int = 3;
    let y: &int = &x;
    assert!(*y == 3);
    // assert!(y == 3); /* Does not type-check */

    struct Pair<A,B> { a: A, b: B }
    let p = Pair { a: 4, b: "hi" };
    let y: &int = &p.a;
    let (z1, z2) = (y, y); // &T impl's Copy for any T.
    assert!(*y == 4);
    // SLIDE FINIS
}
RUST
 ) 

(slide-code/url "&mut T :  mutable unaliased reference"
 #<<RUST
fn main() {
    // SLIDE BEGIN
    let mut x: int = 5;
    {
        let y = &mut x;
        increment(y);
    }
    assert!(x == 6);

    fn increment(r: &mut int) {
        *r = *r + 1;
    }

    let y = &mut x;
    // let (z1, z2) = (y, y); /* Does not type-check */
    // SLIDE FINIS
}
RUST
)

(slide-code/url "pattern matching and refs: Why"
 #<<RUST
struct Pair<A,B> { a: A, b: B }
fn add_b_twice<T>(p: Pair<int,T>,
                  f: fn (&T) -> int) -> int {
  match p {
    Pair{ a, b } => {
        //   ^ `p.b` is moved into `b` here, so
        // cannot compile: use of moved value: `p.b`
        a + f(&b) + f(&p.b)
    }
  }
}
// SLIDE FINIS
fn main() {} 
RUST
)

(slide-code/url "pattern matching and refs: How"
 #<<RUST
struct Pair<A,B> { a: A, b: B }
fn add_b_twice<T>(p: Pair<int,T>,
                  f: fn (&T) -> int) -> int {
  match p {
    Pair{ a, ref b } => {
        //   ^ now `p.b` is left in place, and
        // `b` is bound to a `&T` instead of a `T`.
        // (even happens when `p` is Copy!)
        a + f(b) + f(&p.b)
    }
  }
}
// SLIDE FINIS
fn main() {
    fn extract(b: &Box<int>) -> int { **b }
    let p = Pair { a: 3, b: box 4 };
    let answer = add_b_twice(p, extract);
    println!("answer: {}", answer);
}
RUST
)
