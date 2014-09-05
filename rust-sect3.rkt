#lang racket
(require slideshow)
(require unstable/gui/slideshow)
(require "slides-common.rkt")
(require "rust-common.rkt")
(require "slide-rust-code.rkt")

(slide #:name "Isn't linearity a pain?"
       (para "Is every kind of mutability forced into a linearly passed type?"))

(slide #:name "Two kinds of mutation"
       (para "There two kinds of mutatibility in Rust:")
       (item "Inherited")
       (item "Interior")
       (para "(remember: goal is to prevent data-races; not to hamstring developers)"))

(define (inherited-mutability-example line-3 line-2 line-1)
     (let* ((template #<<RUST
 fn main() {
     #[deriving(Show)]
     // SLIDE BEGIN
     struct S { x: int, y: int }
     let a = S { x: 3, y: 4 }; // a.x and a.y *immutable*
     let mut b = a;           // b.x and b.y are mutable
     let mut c = b;           // c.x and c.y are mutable
     b.x = 5; b.y = 6;

     { let u = { #[deriving(Show)] // SLIDE HIDE
     $$LINE_3$$
     $$LINE_2$$
     $$LINE_1$$
     // SLIDE FINIS
     u
     };
     println!("u: {}", u);
     }
     println!("a: {}", a);
     println!("b: {}", b);
     println!("c: {}", c);
}
RUST
             )
            (template (string-replace template "$$LINE_3$$" line-3))
            (template (string-replace template "$$LINE_2$$" line-2))
            (template (string-replace template "$$LINE_1$$" line-1)))
      template))

(slide-code/tiny-url
"http://is.gd/ZUj4Jg"
"Inherited mutability" (inherited-mutability-example
                                        "struct T<'l> { p: &'l S,\n                    q: &'l mut S }"
                                        "let u;\nu = T { p: &b, q: &mut c };"
                                        "u.p.x = 7;"))

(slide-code/tiny-url/punchline
 "http://is.gd/Yb0Zg2"
 "Inherited mutability"
 (inherited-mutability-example "struct T<'l> { p: &'l S,\n                    q: &'l mut S }"

                               "let u;// COLOR:red\nu = T { p: &b, q: &mut c };"
                               "u.p.x = 7; // COLOR:red")
 (para "you can't mutate" (tt "u.p.x") "this way;" (tt "u") "is not marked" (tt "mut")))

(slide-code/tiny-url
 "http://is.gd/qqlCfu"
 "Inherited mutability"
 (inherited-mutability-example "struct T<'l> { p: &'l S,\n                    q: &'l mut S }"
                               "let mut u;\nu = T { p: &b, q: &mut c };"
                               "u.p.x = 7;"))

(slide-code/tiny-url/punchline
 "http://is.gd/lCtD1D"
 "Inherited mutability"
 (inherited-mutability-example "struct T<'l> { p: &'l S, // COLOR:red\n                    q: &'l mut S }"
                               "let mut u;\nu = T { p: &b, q: &mut c };"
                               "u.p.x = 7; // COLOR:red")
 (para "you can't mutate" (tt "u.p.x") "this way;" (tt "u.p") "is not" (tt "&mut")))

(slide-code/tiny-url/punchline
 "http://is.gd/BlhZ4O"
 "Inherited mutability"
 (inherited-mutability-example "struct T<'l> { p: &'l S,\n                    q: &'l mut S }"
                               "let mut u;\nu = T { p: &b, q: &mut c };"
                               "u.q.x = 7;")
 (para "(this one works; path all way down" (tt "u.q.x") "is marked" (tt "mut") ")"))

(slide #:title "Inherited mutability in summary"
       (para "Inherited mutability: if you own something or have exclusive"
             "access to it, you can modify it.")
       (item "You just might need to move"
             "it into a local slot marked" (tt "mut") "first.")
       'next
       (item "But the inverse does not hold!"))

(slide #:title "Interior mutability"
       (para "Library types" (tt "Cell") "and" (tt "RefCell")
             "can be modified via" (bold "shared") "references")
       (item (tt "Cell<T>") ": provides" (tt "get") "and" (tt "set") "methods")
       (subitem "easy access, but only usable for" (tt "T : Copy"))
       (subitem "for other types, must use ...")
       'next
       (item (tt "RefCell<T>") ": you claim temporary exclusive access.")
       'next
       (subitem "Compiler does not check your claim!")
       (subitem "Dynamically checked; task failure if you broke rules."))

;; This:
;;
;;    http://is.gd/pieVl6
;;
;; is a more honest recreation of the original.  but I think I
;; couldn't fit it on the slide.  And then I goofed during the
;; presentation and made a misstatement about the relationship
;; of `Copy` and top-level `fn` items.

(slide-code/tiny-url/punchline
 "http://is.gd/bfXxT0"
 "Demo of Cell<T>"
 #<<RUST
use std::cell::Cell;
fn main() {
    // SLIDE START
    enum E { A(int), B(int) }
    let a = Cell::new(A(16)); let b = Cell::new(B(17));
    let c1 = &a;              let _c2 = &b;
    foo(c1, c1);

    fn foo(p1: &Cell<E>, p2: &Cell<E>) {
        p2.set(B(0xBaC0de));
        match p1.get() {
            B(ref val) | A(ref val) => {
                println!("{:X}", *val);
            }
        }
    }
    // SLIDE FINIS
}
RUST
 (para "(this is" (bold "fine") "!)")
 )

(slide #:title "Back to inherited vs interior"
 (para "So what does the" (tt "mut") "marker actually mean?")
 'next
 (item (tt "mut") " does not mark all and only \"mutable things\".")
 (subitem "It marks the items for which, at some point,"
  "someone wants *exclusive access*" "to do some operation (often mutation)."))
