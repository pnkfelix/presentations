#lang racket
(require slideshow)
(require "rust-common.rkt")

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

(slide
 #:title "The same puzzle in Rust"
 (vl-append
  (t "Rust:")
  (rust-tt/nl #<<RUST
use std::mem::size_of;
enum Lonely<A> { One(A), Two(A, A) }
let size =
    size_of::<Lonely<(int,int,int,int,int)>>();
let word_size = size_of::<int>();
println!("words: {}", size / word_size);
RUST
))
 'next
 (item "Prints " (tt "words: 11"))
 (item "Here is how Rust represents a " (tt "Two"))
 rendering-of-rust-lonely-tuples
 'next
 (item "Here is how Rust represents a " (tt "One"))
 rendering-of-rust-lonely-tuple
 )

(define (big-t s)
  (text s (current-main-font) 48))

(slide #:name "Implications of move semantics"
       'alts
       (list (list (big-t "Implications"))))

(slide
 #:title "To Move or To Copy?"
 (item "This does not compile")
(rust-tt/nl #<<RUST
fn twice<T:Show>(x: T, f: fn (T) -> T) -> T {
    let w = f(x);
    println!("temp w: {}", w);
    let y = f(x);
    println!("temp y: {}", y);
    let z = f(y); return z;
}
RUST
)
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
))

(slide #:name "Why all the fuss about move semantics?"
       'alts
       (list (list (big-t "Why all the fuss about move semantics?"))))

(outline 'three)

(slide
 #:title "Rust: Values and References"
 (item "Life outside of ref-cells")
 (item "There are three core types" (tt "T") "to think about.")
 (item (tt "     T") "  non-reference")
 (comment "e.g. ints, tuples, struct instances, ...")
 (item (tt "    &T") "  shared reference")
 (item (tt "&mut T") "  mutable unaliased reference")
 'next
 (item (tt "    *T") "  too (unsafe pointers); not this talk")
 )


(slide
 #:title "&T : shared reference"
 (rust-tt/nl #<<RUST
let x: int = 3;
let y: &int = &x;
assert!(*y == 3);
// assert!(y == 3); /* Does not type-check */

struct Pair<A,B> { a: A, b: B }
let p = Pair { a: 4, b: "hi" };
let y: &int = &p.a;
assert!(*y == 4);
RUST
))

(slide #:title "&mut T :  mutable unaliased reference"
 (rust-tt/nl #<<RUST
let mut x: int = 5;
increment(&mut x);
assert!(x == 6);

fn increment(r: &mut int) {
    *r = *r + 1;
}
RUST
))

(slide
 #:title "pattern matching and refs: Why"
 (rust-tt/nl #<<RUST
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
RUST
))

(slide
 #:title "pattern matching and refs: How"
 (rust-tt/nl #<<RUST
struct Pair<A,B> { a: A, b: B }
fn add_b_twice<T>(p: Pair<int,T>,
                  f: fn (&T) -> int) -> int {
  match p {
    Pair{ a, ref b } => {
        //   ^ now `p.b` is left in place, and
        // `b` is bound to a `&T` instead of a `T`.
        a + f(b) + f(&p.b)
    }
  }
}
RUST
))

(slide #:name "Why all the fuss about aliasing?"
       'alts
       (list (list (big-t "Why all the fuss about aliasing?"))
             (list (big-t "It is for type soundness"))))

(slide #:title "mutable aliasing ⇒ soundness holes"
 (rust-tt/nl #<<RUST
fn add3(x:int) -> int { x + 3 }
enum E { A(fn (int) -> int), B(int) }
let mut a = A(add3); let mut b = B(17);
let p1 = &mut a;     let p2 = &mut b;
foo(p1, p2);

fn foo(p1: &mut E, p2: &mut E) {
  match p1 {
      &B(..) => fail!("cannot happen"),
      &A(ref adder) => {
          *p2 = B(0xdeadc0de);
          println!("{}", (*adder)(14));
      }
  }
}
RUST
)
 'next
 (item "(punchline: above is fine;" (tt "rustc") "accepts it)")
 )

(slide #:title "mutable aliasing ⇒ soundness holes"
 (rust-tt/nl #<<RUST
fn add3(x:int) -> int { x + 3 }
enum E { A(fn (int) -> int), B(int) }
let mut a = A(add3); let mut b = B(17);
let p1 = &mut a;     let p2 = &mut b;
foo(p1, p2);

fn foo(p1: &mut E, p2: &mut E) {
  match p1 {
      &B(..) => fail!("cannot happen"),
      &A(ref adder) => {
          *p1 = B(0xdeadc0de);
          println!("{}", (*adder)(14));
      }
  }
}
RUST
)
 'next
 (item "(punchline: above is badness;" (tt "rustc") "rejects it)")
 )

(slide #:title "mutable aliasing ⇒ soundness holes"
 (rust-tt/nl #<<RUST
fn add3(x:int) -> int { x + 3 }
enum E { A(fn (int) -> int), B(int) }
let mut a = A(add3); let mut b = B(17);
let p1 = &mut a;     let p2 = &mut b;
foo(p1, p1);

fn foo(p1: &mut E, p2: &mut E) {
  match p1 {
      &B(..) => fail!("cannot happen"),
      &A(ref adder) => {
          *p2 = B(0xdeadc0de);
          println!("{}", (*adder)(14));
      }
  }
}
RUST
)
 'next
 (item "(punchline: above is badness;" (tt "rustc") "rejects it)")
 )


(slide #:name "Why all the fuss about move semantics revisited?"
       'alts
       (list (list (big-t "Why all the fuss about move semantics?"))
             (list (big-t "Allows us to reason about aliasing"))))
