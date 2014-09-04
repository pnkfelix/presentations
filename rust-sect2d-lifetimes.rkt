#lang racket
(require slideshow)
(require "slides-common.rkt")
(require "rust-common.rkt")
(require "playpen.rkt")
(require "slide-rust-code.rkt")

(slide #:title "Lifetimes"
       (item "Earlier claim:" (tt "&T") "is shared reference")
       (item "Does this mean you can copy it anywhere you like?")
       'next
       (item "Of course not; must disallow dangling pointers"))

(slide #:title "Reality of types"
       (item "Core types are not really" (tt "&T") "and" (tt "&mut T"))
       'next
       (item "They are really" (tt "&'a T") "and" (tt "&'b mut T"))
       (subitem (tt "'IDENT") "is distinguished syntax for lifetime parameters")
       (subitem "(similar to \"regions\" used by Tofte/Talpin 1994)")
       'next
       (item (tt "x: &'a T") "means x is a reference that will survive at *least* as long as"
             (tt "'a") "and perhaps longer")
       (subitem "implicitly," (tt "T") "itself must also live that long"))

(slide #:title "Reality of functions"
       (para "A function")
       (rust-tt "fn foo(x: &int, y: &int)")
       (para "is really sugar for the more explicit form")
       (rust-tt "fn foo<'a,'b>(x: &'a int, y: &'b int)")
       'next
       (para "You can also put in bounds:")
       (rust-tt "fn baz<'a,'b:'a>(x: &'a int, y: &'b int)")
       (para "meaning" (tt "'b") "lives at least as long as" (tt "'a")
             "(and perhaps longer)"))

(call-with-url-and-code
 #<<RUST
fn main() {
    // SLIDE BEGIN
    let y : int = 3;
    #[deriving(Show)] // SLIDE HIDE
    struct S<'a> { x: &'a int }
    let z : S   = S { x: &y };
    // SLIDE FINIS
    println!("z: {}", z);
}
RUST
 (lambda (url code)
  (slide #:title "Reality of structs / enums"
   (item "Did you notice that none of the examples put references inside structs?")
   'next
   (item "Rust requires explicit lifetimes on reference-types in fields.")
   (subitem "which effectively means such structs need to be lifetime-parametric")
   'next
   (frame code)
   url)))
       
