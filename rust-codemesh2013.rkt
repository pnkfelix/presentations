#lang racket
;#lang slideshow
(require slideshow)

(string-append "Programming languages are in constant development, "
               "responding to the changing nature of computing "
               "problems and hardware infrastructure. ")

#;(begin 
  (set! actual-screen-w 800)
  (set! actual-screen-h 600))

(current-main-font "Open Sans")

;; string -> [listof string)
(define (fragment-at-newlines str)
  ;; [listof char] [listof char] -> [listof [listof char]]
  (define (fragment accum chars)
    (cond ((null? chars) 
           (cond ((null? accum) '())
                 (else (list (reverse accum)))))
          (else 
           (cond ((char=? (car chars) #\newline)
                  (cons (reverse accum) (fragment '() (cdr chars))))
                 (else
                  (fragment (cons (car chars) accum) (cdr chars)))))))
  (map list->string (fragment '() (string->list str))))
                       
(define (tt/nl string)
  (apply vl-append (map tt (fragment-at-newlines string))))

; (slide (t "Hello World"))
; (slide (t "fn main() { println!(\"Hello World\"); }"))
; (slide (t/nl "fn main() {\n    println!(\"Hello World\");\n}"))

#;(current-slide-assembler (lambda (s v-sep c) c))

(define outline
  (let ((sub-para (lambda l (para #:width (* 3/4 (current-para-width)) l))))
    (make-outline
     'one   "Part I: Motivation"
     (lambda (tag) (sub-para "Why Mozilla is investing in Rust"))
     
     'two   "Part II: Rust syntax and semantics"
     (lambda (tag)
       (sub-para "Systems programming under the influence of FP")
       #;(sub-para "Standing on the shoulders of giants"))
     
     'three "Part III: Ownership and borrowing"
     (lambda (tag) (sub-para "How Rust handles pointers"))
     
     'four  "Part IV: Concurrency model"
     (lambda (tag) (sub-para "Communication between tasks")))))

(slide (scale (bitmap "rust_logo-only_RGB.png") 0.3)
       (scale (bitmap "mozilla_wordmark_CS4.png") 0.1))

 ;; TODO: Overview of what Rust offers (one slide), then transition to the "Why"

(slide #:title "Motivation"
       (item "Why Mozilla is investing in a new programming language")
       'next
       (item "Web browsers are complex programs")
       (item "It is expensive to innovate and compete while implementing atop standard systems languages")
       'next
       (item "So for our experimental next-generation browser, Servo ...")
       (subitem "http://github.com/mozilla/servo")
       'next
       (item "... our research team is using a new programming language: Rust")
       (subitem "http://rust-lang.org"))

(outline 'one)

(slide #:title "Language Design"
       (item "Goal: bridge performance gap between safe and unsafe languages")
       (item "Design choices largely fell out of that requirement")
       (item "Rust compiler, stdlib, and tools are all MIT/Apache dual license."))

(slide #:title "Systems Programming"
       (item "Resource-constrained enviroments, direct control over hardware")
       (item "C and C++ dominate this space")
       (item "Systems programmers care about the last 10-15% of potential performance"))
 
(slide #:title "Unsafe aspects of C"
       (item "Dangling pointers")
       (item "Null pointer dereferences")
       (item "Buffer overflows, array bounds errors")
       (item "Format string and argument mismatch")
       (item "Double frees"))

(require slideshow/balloon)
(slide #:title "Tool: Sound Type Checking"
       
       (item (let ((orig (t "''Well-typed programs don't go wrong.''")))
               (pin-balloon (wrap-balloon (t "Milner, 1978") 'sw 0 0) orig (pict-width orig) 0)))
       (comment "Robin Milner, ''A theory of type polymorphism in programming'' 1978")
       'next
       (item "More generally: identify classes of errors ...")
       (subitem "... then use type system to remove them")
       (subitem "(or at least isolate them)")
       (item "Eases reasoning about programs; provides more confidence in their correctness.")
       'next
       (item "Revised: Well-typed programs can assist in blame assignment.")
       (subitem "(unsafe code remains a potential way to ``go wrong'')")
       (subitem "and even safe code can fail; but only in ways one can reason about and recover from")
       )

(slide #:title "Simple source ⇔ compiled code relationship"
       (item "This is a reason C persists to this day")
       (item "Programmer can build mental model of machine state")
       (comment "especially with respect to memory")
       (item "Programmer can also control low-level details (e.g. memory layout)")
       (item "Goal: Rust should preserve this relationship ...")
       'next
       (subitem "... while" (bt "retaining") "memory safety ...")
       (comment "One definition of memory safety: programs can only dereference"
                "pointers to previously allocated memory that has not yet been"
                "freed.")
       'next
       (subitem "... without runtime cost.")
       (comment "In languages like Java and Haskell, safe abstractions have runtime costs:"
                "boxing everything; garbage-collecting everything")
       )

(slide #:title "Zero-cost abstractions"
       (comment "Cite Robert O'Callahan if possible")
       (item "Goal: do not pay at runtime for a feature unused by program")
       (item "There is still a non-zero cognitive cost")
       (subitem "Often must think more about data representation")
       (subitem "Make choices about memory allocation")
       (item "But in safe blocks of code, compiler checks our assumptions"))



(outline 'two)

(define (slide-expression-oriented ghosted)
  (define (maybe-ghost i) (if ghosted (ghost i) i))
  (define (maybe-next) (if ghosted (comment "next") 'next))
  (slide #:title "Expression-oriented"
         (vc-append (* gap-size 2)
                    (item "not statement-oriented (unless you want to be)")
                    (maybe-ghost (item (t "An expression:") (tt "2 + 3 > 5"))))
         (maybe-ghost (item  (t "An expression:") (tt "{ let x = 2 + 3; x > 5 }")))
         (maybe-next)
         (item (vl-append (maybe-ghost (hbl-append (t "A binding of ") (tt "y") (t " followed by an expression:")))
                          (tt/nl #<<HERE
let y = { let x = 2 + 3; x > 5 };
if y { x + 6 } else { x + 7 }
HERE
                                 )))
       (maybe-next)
       (item (vl-append (maybe-ghost (t "Function definition and invocation"))
                        (tt "fn add3(x:int) -> int { x + 3 }")
                        (maybe-ghost (tt "let y = foo(2) > 5;"))))
       ))

(slide-expression-oriented #f)
(slide-expression-oriented 'ghosted)

(slide #:title "Expression-oriented"
       (vc-append (item "not statement-oriented (unless you want to be)"))           
       (item (vl-append (tt/nl #<<HERE
let y = { let x = 2 + 3; x > 5 };
if y { x + 6 } else { x + 7 }
HERE
                    )))
       
       (item (vl-append (tt "fn add3(x:int) -> int { x + 3 }")))
       'next
       (item (hbl-append (t "But ") (tt "return") (t " statement is available if you prefer that style")))
       (vl-append
        (tt "fn add3(x:int) -> int { return x + 3; }")
        (tt "")
        (tt/nl #<<HERE
let y = { let x = 2 + 3; x > 5 };
if y {
  return x + 6;
} else {
  return x + 7;
}
HERE
       ))
       #;'next
       #;(item "Statements are semi-colon terminated;``unit'' type is denoted by " (tt "()"))
       (comment "treated same as expressions with the uninteresting ``unit'' type:" (tt "()"))
       (comment "No whitespace sensitivity for lexing/parsing")
       (comment "If a fn has unit return type, that portion of the type signature can be omitted.")
       )

(slide #:title "Syntax extensions"
       (item "C has a preprocessor")
       (subitem "can define ``macros'' via " (tt "#define")
                " to abstract over syntactic patterns in the code")
       (item "Likewise, Rust has its own syntax extension mechanism")
       'next
       (item "Macro-invocations in Rust look like " (tt "macroname!(...)"))
       (subitem "All macro-uses have an exclamation point, "
                "to ease lexical analysis (for humans and simple-minded programming tools)")
       (tt/nl #<<HERE
println!("Hello World {:d}", some_int);
assert!(some_int == 17);
HERE
              )
       'next
       (item "(User-defined macros are out of scope of talk)")
)

(slide #:title "Mutability"
       (item "Local state is immutable by default")
       (vl-append
        (tt/nl #<<HERE
let x = 5;
let mut y = 6;
y = x;     // fine
HERE
              )
       (colorize (tt #<<HERE
x = x + 1; // static error!
HERE
                     ) "red"))
       (comment "mutability-by-accident is huge source of bugs")
       )

(define (side-by-side label-a code-a
                      space
                      label-b code-b)
  (htl-append (vc-append code-a (t " ") label-a)
              space
              (vc-append code-b (t " ") label-b)))

(slide #:title "Enumerated variants I"
       (side-by-side (t "Rust enum") (tt/nl #<<RUST_ENUM
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
       (side-by-side (t "Rust match") (tt/nl #<<RUST_MATCH
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
       (side-by-side (colorize (t "Rust type error") "red") (tt/nl #<<RUST_MATCH
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
       (tt/nl #<<RUST_ENUM
enum Spot {
    One(int)
    Two(int, int)
}
RUST_ENUM
))

(slide #:title "Destructuring match"
       (tt/nl #<<RUST_MATCH
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
       (tt/nl #<<RUST_STRUCT
struct IntPair { x: int, y: int }

let p34 = IntPair{ x: 3, y: 4 };

fn zero_x(p: IntPair) -> IntPair {
  return IntPair{ x: 0, ..p };
}
RUST_STRUCT
              ))

(slide #:title "Closures"
       (item "Rust offers C-style function-pointers that carry no environment")
       (item "Rust also offers function closures that capture relevant portions of their environment")
       (item "Syntax is inspired by Ruby blocks")
       'next
       (tt/nl #<<RUST_CLOSURE
let p34 = IntPair{ x: 3, y: 4 };
let x_adjuster =
  |new_x| { IntPair{ x: new_x, ..p34 } };

let p14 = x_adjuster(1);
let p24 = x_adjuster(2);
println!("p34.x: {} p14.x: {}", p34.x, p14.x);
RUST_CLOSURE
              )
       'next
       (vc-append (t "prints") (tt "p34.x: 3 p14.x: 1")))

(slide #:title "What about OOP?"
       (item "Rust has methods too, and interfaces")
       (item "They require we first explore Rust's notion of a ``pointer''"))

(slide #:title "Pointers"
       (tt/nl #<<HERE
let x: int = 3;
let y: &int = &x;
assert!(*y == 3);

// assert!(y == 3); /* Does not type-check */
HERE
              ))

(slide #:title "Pointers and Mutability"
       (tt/nl #<<HERE
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
       (subitem "uniquely-owned memory on an ``exchange heap''")
       (subitem "intra-task shared memory on a managed (GC) heap")
       'next
       (item "The type system allows code to ``borrow'' references to (and into)"
             "owned memory, and tracks that none of its static safety rules are violated")
       (subitem "You can also borrow references to the GC heap")
       (subitem "but in that case Rust sometimes resorts to dynamic enforcement of the borrowing rules")
       )

(slide #:title "Methods"
       (tt/nl #<<RUST_METHODS_DEF
struct IntPair { x: int, y: int }

impl IntPair {
  fn zeroed_x_copy(self) -> IntPair {
    return IntPair { x: 0, ..self }
  }

  fn replace_x(&mut self) { self.x = 0; }
}
RUST_METHODS_DEF
              )
       'next
       (tt/nl #<<RUST_METHODS_USE
let mut p_tmp = IntPair{ x: 5, y: 6 };
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
       (tt/nl #<<RUST_GENERICS
enum Option<T> {
  Some(T),
  None
}
RUST_GENERICS
              )
       'next
       (tt/nl
               
              #<<RUST_GENERICS
fn safe_get<T>(opt: Option<T>, dflt: T) -> T {
  match opt {
    Some(contents) => contents,
    None           => dflt
  }
}
RUST_GENERICS
              ))

(define cake-rust-example
  '((cake-data #<<CAKE_DATA
enum Flavor { chocolate, vanilla }
struct Cake { flavor: Flavor, num_slices: int }
CAKE_DATA
               )
    
    (cake-funs #<<HERE
fn birthday_cake(f:Flavor, num_slices:int) -> Cake;
fn status(cake: &Cake, when: &str);
fn eat_entire(cake: &mut Cake)

// On return, ate >= `count` (or cake is gone).
fn eat_at_least(cake: &mut Cake, count: &int)
HERE
               )
    
    (eat-slice #<<CAKE_METHODS
impl Cake {
    fn eat_slice(&mut self) {
        self.num_slices -= 1;
    }
}
CAKE_METHODS
                  )
    (eat-at-least #<<CAKE_FUNCTIONS
fn eat_at_least(cake: &mut Cake, threshold: &int) {
    let mut eaten_so_far = 0;
    while (cake.num_slices > 0
           && eaten_so_far < *threshold) {
        cake.eat_slice();
        eaten_so_far += 1;
    }
}
CAKE_FUNCTIONS
                  )
    (eat-entire-buggy #<<CAKE_FUNCTIONS
fn eat_entire(cake: &mut Cake) {
    eat_at_least(cake, &cake.num_slices);
}
CAKE_FUNCTIONS
                      )
    (eat-entire-fixed #<<CAKE_FUNCTIONS
fn eat_entire(cake: &mut Cake) {
    let n = cake.num_slices;
    eat_at_least(cake, &n);
}
CAKE_FUNCTIONS
                      )
    (main #<<CAKE_FUNCTIONS
fn main () {
    let mut cake = birthday_cake(vanilla, 16);
    status(&cake, "at outset");
    eat_at_least(&mut cake, &2);
    status(&cake, "after 2");
    eat_entire(&mut cake);
    status(&cake, "finally");
}
CAKE_FUNCTIONS
          )
    (status #<<CAKE_FUNCTIONS
fn status(cake: &Cake, when: &str) {
    println!("cake {} has {} slices.", when, cake.num_slices);
}
CAKE_FUNCTIONS
            )
    (bday-cake #<<CAKE_FUNCTIONS
fn birthday_cake(f:Flavor, num_slices:int) -> Cake {
    Cake { flavor: f, num_slices: num_slices }
}
CAKE_FUNCTIONS
               )
    ))
  
(define cake-c++-example
  '((includes  "#include <iostream>")
    (cake-data #<<HERE
enum Flavor { chocolate, vanilla };
struct Cake {
    Flavor flavor; int num_slices;
    void eat_slice();
};
HERE
                 )
    (cake-funs #<<HERE
Cake birthday_cake(Flavor f, int num_slices);
void print_status(Cake const &cake, std::string);
void eat_entire(Cake &cake);

// On return, ate >= `count` (or cake is gone).
void eat_at_least(Cake &cake, int const &count);
HERE
               )
    (eat-slice "void Cake::eat_slice() { this->num_slices -= 1; }")
    (eat-at-least #<<HERE
void eat_at_least(Cake &cake, int const &threshold)
{
    int eaten_so_far = 0;
    while (cake.num_slices > 0
           && eaten_so_far < threshold) {
        cake.eat_slice();
        eaten_so_far += 1;
    }
}
HERE
                  )
    (eat-entire #<<HERE
void eat_entire(Cake &cake) {
    eat_at_least(cake, cake.num_slices);
}
HERE
                )
    (main #<<HERE
int main () {
    Cake cake = birthday_cake(vanilla, 16);
    print_status(cake, "at outset");
    eat_at_least(cake, 2);
    print_status(cake, "after 2");
    eat_entire(cake);
    print_status(cake, "finally");
}
HERE
          )
    (status #<<HERE
void status(Cake const &cake, std::string when) {
    std::cout << "cake " << when
              << " has " << cake.num_slices << " slices." << std::endl;
}
HERE
            )
    (bday-cake #<<HERE
Cake birthday_cake(Flavor f, int num_slices) {
    Cake c;
    c.flavor = f;
    c.num_slices = num_slices;
    return c;
}
HERE
               )
    ))

(define cake-c++-main-transcript #<<HERE
cake at outset has 16 slices.
cake after 2 has 14 slices.
cake finally has 7 slices.
HERE
  )
  
(define cake-rust-names (map car cake-rust-example))
(define cake-c++-names (map car cake-c++-example))
  
(define (rust-code-for example name-of-part)
  (cadr (assq name-of-part example)))

(define (c++-code-for example name-of-part)
  (cadr (assq name-of-part example)))

(define (cake-rust-code-for name)
  (rust-code-for cake-rust-example name))
  
(define (cake-c++-code-for name)
  (c++-code-for cake-c++-example name))
  
(define (generalized-code-tt example specs code-for)
  (letrec ((from-name (lambda (name) (tt/nl (code-for example name))))
           (from-spec (lambda (spec)
                        (cond ((symbol? spec) (from-name spec))
                              ((eq? 'ghost (car spec)) (ghost (from-spec (cadr spec))))))))
    (apply vl-append gap-size (map from-spec specs))))

(define (c++-code-tt example specs)
  (generalized-code-tt example specs c++-code-for))

(define (rust-code-tt example specs)
  (generalized-code-tt example specs rust-code-for))

(define (c++-code-slide example specs)
  ;(slide #:layout 'top (c++-code-tt example specs))
  (slide (lt-superimpose (c++-code-tt example specs) full-page))
  )

(define (rust-code-slide example specs)
  ;(slide #:layout 'top (c++-code-tt example specs))
  (slide (lt-superimpose (rust-code-tt example specs) full-page))
  )

(define (big-t s)
  (text s (current-main-font) 48))

(slide #:name "An example"
       'alts
       (list (list (big-t "An example from C/C++"))
             (list (big-t "A (contrived, strawman) example from C/C++"))))
       
(c++-code-slide cake-c++-example '(cake-data))
(c++-code-slide cake-c++-example '(cake-data cake-funs))
(c++-code-slide cake-c++-example '((ghost cake-data) cake-funs))
(c++-code-slide cake-c++-example '(cake-funs))
(c++-code-slide cake-c++-example '(cake-funs eat-slice))
(c++-code-slide cake-c++-example '(cake-funs eat-slice eat-at-least))
(c++-code-slide cake-c++-example '(cake-funs))
(c++-code-slide cake-c++-example '(cake-funs eat-entire))
(c++-code-slide cake-c++-example '(cake-funs eat-entire main))
(c++-code-slide cake-c++-example '((ghost cake-funs) (ghost eat-entire) main))

(slide #:layout 'top
       (c++-code-tt cake-c++-example '(main))
       'next
       (t "Transcript of run:")
       (frame (tt/nl cake-c++-main-transcript))
       'next
       (t "Oops."))

(slide #:layout 'top
       (c++-code-tt cake-c++-example '(eat-at-least eat-entire))
       'next
       (t "Classic aliasing bug"))

(slide
 (t "The previous example was contrived, but aliasing bugs are real")
 (t "They can lead to crashes, security holes, and just general incorrect behavior")
 'next
 (t "We want Rust to make it harder to make silly mistakes.")
 'next
 (t "(but not impossible)")
 'next
 'alts
 (list (list
        (hbl-append (t "((you need to opt in to write unsafe code))")))
       (list
        (hbl-append (t "((you need to opt in to write ") (tt "unsafe") (t " code))")))))


(slide #:name "The Cake example in Rust"
 (t "What does the Cake code look like in Rust?"))

(rust-code-slide cake-rust-example '(cake-data))
(rust-code-slide cake-rust-example '(cake-data cake-funs))
(rust-code-slide cake-rust-example '((ghost cake-data) cake-funs))
(rust-code-slide cake-rust-example '(cake-funs))
(rust-code-slide cake-rust-example '(cake-funs eat-slice))
(rust-code-slide cake-rust-example '(cake-funs eat-slice eat-at-least))
(rust-code-slide cake-rust-example '(cake-funs))
(rust-code-slide cake-rust-example '(cake-funs eat-entire-buggy))
(rust-code-slide cake-rust-example '(cake-funs eat-entire-buggy main))
;;(rust-code-slide cake-rust-example '((ghost cake-funs) (ghost eat-entire-buggy) main))

(slide #:name "Cake in Rust: Punchline"
       (item "So, wait, was the port successful?")
       'next
       (tt/nl #<<RUSTC_ERROR_MSG
% rustc cake.rs
error: cannot borrow `(*cake).num_slices` as
       immutable because it is also borrowed
       as mutable
eat_at_least(cake, &cake.num_slices);
                   ^~~~~~~~~~~~~~~~
note: second borrow of `(*cake).num_slices`
      occurs here
eat_at_least(cake, &cake.num_slices);
             ^~~~
error: aborting due to previous error
RUSTC_ERROR_MSG
              ))

(slide #:layout 'top
       (colorize (rust-code-tt cake-rust-example '(eat-entire-buggy)) "red")
       'next
       (t "The compiler is complaining about our attempt to alias here!")
       'next
       (item "This fixed version compiles fine. ")
       (rust-code-tt cake-rust-example '(eat-entire-fixed))
       'next
       (para "Of course, this fix is applicable to our C++ code too. "
             "The point is that Rust enforces these stricter rules outlawing borrows that alias"
             "(at least in safe code).")
       )


(slide #:name "Concurrency"
       (t "Concurrency"))


;; AHHH XXX FIXME

(define dherman-slide-factor 0.5)
(slide (scale (bitmap "dherman-slide29.png") dherman-slide-factor))
(slide (scale (bitmap "dherman-slide30.png") dherman-slide-factor))
(slide (scale (bitmap "dherman-slide31.png") dherman-slide-factor))
(slide (scale (bitmap "dherman-slide32.png") dherman-slide-factor))
(slide (scale (bitmap "dherman-slide33.png") dherman-slide-factor))
(slide (scale (bitmap "dherman-slide34.png") dherman-slide-factor))
(slide (scale (bitmap "dherman-slide35.png") dherman-slide-factor))


;; AAHHH XXX FIXME

(slide #:title "Topics not covered"
       (item "regions/lifetimes and their subtyping relationship")
       (item "traits as existentials (object-oriented dispatch)")
       (item "borrow-checking static analysis rules")
       (item "freezing/thawing data structures")
       (item "task-local storage")
       (item "linked-failure")
       (item "one-shot closures: " (tt "proc"))
       (item "syntax extensions"))

(slide #:name "Our community"
       (para "The Rust team: "
             "Brian Anderson, " 
             "Alex Chrichton, "
             "Felix Klock (me), "
             "Niko Matsakis, "           
             "Patrick Walton")
       (para "Interns/Alumni: "
             "Graydon Hoare, "
             "Michael Bebenita," "Ben Blum,"
             "Tim Chevalier,"
             "Rafael Espíndola,"
             "Roy Frostig, " "Marijn Haverbeke, " "Eric Holk, "
             "Lindsey Kuper, " "Elliott Slaughter, " "Paul Stansifer, "
             "Michael Sullivan")
       (t "(and the many members of the larger Rust community)")
       (t "")
       (t "http://rust-lang.org/"))

(slide #:title "Join the Fun!"
       (tt "rust-lang.org")
       (scale (bitmap "rust_logo-only_RGB.png") 0.1)
       (hbl-append (t "mailing-list: ") (tt "rust-dev@mozilla.org"))
       (hbl-append (t "community chat: ") (tt "irc.mozilla.org :: #rust"))
       (scale (bitmap "mozilla_wordmark_CS4.png") 0.1)
       )
