#lang racket
(require slideshow)
(require "rust-codemesh2013-common.rkt")

(outline 'two)

(define (slide-expression-oriented ghosted)
  (define (maybe-ghost i) (if ghosted (ghost i) i))
  (define (maybe-next) (if ghosted (comment "next") 'next))
  (slide #:title "Expression-oriented"
         (vc-append (* gap-size 2)
                    (item "not statement-oriented (unless you want to be)")
                    (maybe-ghost (item (t "An expression:") (rust-tt "2 + 3 > 5"))))
         (maybe-ghost (item  (t "An expression:") (rust-tt "{ let x = 2 + 3; x > 5 }")))
         (maybe-next)
         (item (vl-append (maybe-ghost (hbl-append (t "A binding of ") (rust-tt "y") (t " followed by an expression:")))
                          (rust-tt/nl #<<HERE
let y = { let x = 2 + 3; x > 5 };
if y { x + 6 } else { x + 7 }
HERE
                                 )))
       (maybe-next)
       (item (vl-append (maybe-ghost (t "Function definition and invocation"))
                        (rust-tt "fn add3(x:int) -> int { x + 3 }")
                        (maybe-ghost (rust-tt "let y = foo(2) > 5;"))))
       ))

(slide-expression-oriented #f)
(slide-expression-oriented 'ghosted)

(slide #:title "Expression-oriented"
       (vc-append (item "not statement-oriented (unless you want to be)"))           
       (item (vl-append (rust-tt/nl #<<HERE
let y = { let x = 2 + 3; x > 5 };
if y { x + 6 } else { x + 7 }
HERE
                    )))
       
       (item (vl-append (rust-tt "fn add3(x:int) -> int { x + 3 }")))
       'next
       (item (hbl-append (t "But ") (rust-tt "return") (t " statement is available if you prefer that style")))
       (vl-append
        (rust-tt "fn add3(x:int) -> int { return x + 3; }")
        (rust-tt "")
        (rust-tt/nl #<<HERE
let y = { let x = 2 + 3; x > 5 };
if y {
  return x + 6;
} else {
  return x + 7;
}
HERE
       ))
       #;'next
       #;(item "Statements are semi-colon terminated;``unit'' type is denoted by " (rust-tt "()"))
       (comment "treated same as expressions with the uninteresting ``unit'' type:" (rust-tt "()"))
       (comment "No whitespace sensitivity for lexing/parsing")
       (comment "If a fn has unit return type, that portion of the type signature can be omitted.")
       )

(slide #:title "Syntax extensions"
       (item "C has a preprocessor")
       #;(subitem "can define ``macros'' via " (tt "#define")
                " to abstract over syntactic patterns in the code")
       (item "Likewise, Rust has syntax extensions")
       'next
       (item "Macro-invocations in Rust look like " (rust-tt "macroname!(...)"))
       (subitem "Eases lexical analysis (for simple-minded ...)")
       (rust-tt/nl #<<HERE
println!("Hello World {:d}", some_int);
assert!(some_int == 17);
fail!("Unexpected: {:?}", structure);
HERE
              )
       'next
       (item "(User-defined macros are out of scope of talk)")
)

(slide #:title "Mutability"
       (item "Local state is immutable by default")
       (vl-append
        (rust-tt/nl #<<HERE
let x = 5;
let mut y = 6;
y = x;     // fine
HERE
              )

       (recolorize
(lambda ()
 (rust-tt #<<HERE
x = x + 1; // static error!
HERE
                     )) "red"))
       (comment "mutability-by-accident is huge source of bugs")
       )

(define (side-by-side label-a code-a
                      space
                      label-b code-b)
  (htl-append (vc-append code-a (t " ") label-a)
              space
              (vc-append code-b (t " ") label-b)))

(slide #:title "Enumerated variants I"
       (side-by-side (t "Rust enum") (rust-tt/nl #<<RUST_ENUM
enum Color
{
    Red,
    Green,
    Blue
}
RUST_ENUM
                                        )
                     (tt "       ")
                     (t "C enum") (tt/nl #<<C_ENUM
typedef enum
{
    Red,
    Green,
    Blue
} color_t;
C_ENUM
                          )))

(slide #:title "Matching enums"
       (side-by-side (t "Rust match") (rust-tt/nl #<<RUST_MATCH
fn f(c: Color) {
  match c {
    Red   => /* ... */,
    Green => /* ... */,
    Blue  => /* ... */
  }
}
RUST_MATCH
                                         )
                     (tt "   ")
                     (t "C switch") (tt/nl #<<C_SWITCH
void f(color_t c) {
  switch (c) {
    case Red:   /* ... */
                break;
    case Green: /* ... */
                break;
    case Blue:  /* ... */
                break;
  }
}
C_SWITCH
                                       ))
       (ghost (item "Rust also checks that cases are exhaustive.")))

(slide #:title "Matching nonsense"
       (side-by-side (colorize (t "Rust type error") "red") (rust-tt/nl #<<RUST_MATCH
fn f(c: Color) {
  match c {
    Red   => /* ... */,
    Green => /* ... */,
    17    => /* ... */
  }
}
RUST_MATCH
                                         )
                     (tt "   ")
                     (t "C switch") (tt/nl #<<C_SWITCH
void f(color_t c) {
  switch (c) {
    case Red:   /* ... */
                break;
    case Green: /* ... */
                break;
    case 17:    /* ... */
                break;
  }
}
C_SWITCH
                                       ))
       'next
       (item "Rust also checks that cases are exhaustive."))

(slide #:title "Enumerated variants II: Algebraic Data"
       (rust-tt/nl #<<RUST_ENUM
enum Spot {
    One(int)
    Two(int, int)
}
RUST_ENUM
))

(slide #:title "Destructuring match"
       (rust-tt/nl #<<RUST_MATCH
fn magnitude(x: Spot) -> int {
    One(n)    => n,
    Two(x, y) => (x*x + y*y).sqrt()
}
RUST_MATCH
              ))

(slide #:title "Structured data"
       (item "Similar to " (tt "struct") " in C")
       (subitem "lay out fields in memory in order of declaration")
       (item "Rust forces programmer to initialize the fields before they are read")
       (subitem "(no garbage data allowed from safe code)")
       'next
       (rust-tt/nl #<<RUST_STRUCT
struct Pair { x: int, y: int }

let p34 = Pair{ x: 3, y: 4 };

fn zero_x(p: Pair) -> Pair {
  return Pair{ x: 0, ..p };
}
RUST_STRUCT
              ))

(slide #:title "Closures"
       (item "Rust offers C-style function-pointers that carry no environment")
       (item "Also closures, to capture portions of environment")
       (item "Syntax is inspired by Ruby blocks")
       'next
       (rust-tt/nl #<<RUST_CLOSURE
let p34 = Pair{ x: 3, y: 4 };
let x_adjuster =
  |new_x| { Pair{ x: new_x, ..p34 } };
let p14 = x_adjuster(1);
let p24 = x_adjuster(2);
println!("p34: {:?} p14: {:?}", p34, p14);
RUST_CLOSURE
              )
       'next
       (hbl-append (t " ⇒  ") (tt "p34: Pair{x: 3, y: 4} p14: Pair{x: 1, y: 4}")))

(slide #:title "What about OOP?"
       (item "Rust has methods too, and interfaces")
       (item "They require we first explore Rust's notion of a ``pointer''"))

(slide #:title "Pointers"
       (rust-tt/nl #<<HERE
let x: int = 3;
let y: &int = &x;
assert!(*y == 3);

// assert!(y == 3); /* Does not type-check */
HERE
              ))

(slide #:title "Pointers and Mutability"
       (rust-tt/nl #<<HERE
let mut x: int = 5;
increment(&mut x);
assert!(x == 6);

fn increment(r: &mut int) {
    *r = *r + 1;
}
HERE
              ))

(slide #:title "Ownership and Borrowing"
       (item "Memory allocated by safe Rust code, 3 cases")
       (subitem "stack-allocated local memory")
       (subitem "owned memory on an ``exchange heap''")
       (subitem "intra-task shared memory on managed heap")
       'next
       (item "code can ``borrow'' references to/into"
             "owned memory; static analysis for safety (no aliasing)")
       (subitem "Can also borrow references to the GC heap")
       (subitem "in that case sometimes resort to dynamic enforcement of the borrowing rules")
       )

(slide #:title "Methods"
       (rust-tt/nl #<<RUST_METHODS_DEF
struct Pair { x: int, y: int }

impl Pair {
  fn zeroed_x_copy(self) -> Pair {
    return Pair { x: 0, ..self }
  }

  fn replace_x(&mut self) { self.x = 0; }
}
RUST_METHODS_DEF
              )
       'next
       (rust-tt/nl #<<RUST_METHODS_USE
let mut p_tmp = Pair{ x: 5, y: 6 };
let p06 = p_tmp.zeroed_x_copy();
p_tmp.replace_x(17);
println!("p_tmp.x: {} p06.x: {}", p_tmp.x, p06.x);
RUST_METHODS_USE
              )
       'next
       (vc-append (t "Prints")
                  (tt "p_tmp.x: 17 p06.x: 0")))

(slide #:title "Generics"
       (item "aka Type-Parametericity")
       (item "Functions and data types can be abstracted over types, not just values")
       'next
       (rust-tt/nl #<<RUST_GENERICS
enum Option<T> {
  Some(T),
  None
}
RUST_GENERICS
              )
       'next
       (rust-tt/nl
               
              #<<RUST_GENERICS
fn safe_get<T>(opt: Option<T>, dflt: T) -> T {
  match opt {
    Some(contents) => contents,
    None           => dflt
  }
}
RUST_GENERICS
              ))
