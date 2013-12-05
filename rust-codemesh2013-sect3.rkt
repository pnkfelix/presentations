#lang racket
(require slideshow)
(require "rust-codemesh2013-common.rkt")

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
  
(define (rust-code-tt example specs code-for)
  (letrec ((from-name (lambda (name) (rust-tt/nl (rust-code-for example name))))
           (from-spec (lambda (spec)
                        (cond ((symbol? spec) (from-name spec))
                              ((eq? 'ghost (car spec)) (ghost (from-spec (cadr spec))))))))
    (apply vl-append gap-size (map from-spec specs))))

(define (c++-code-tt example specs code-for)
  (letrec ((from-name (lambda (name) (tt/nl (c++-code-for example name))))
           (from-spec (lambda (spec)
                        (cond ((symbol? spec) (from-name spec))
                              ((eq? 'ghost (car spec)) (ghost (from-spec (cadr spec))))))))
    (apply vl-append gap-size (map from-spec specs))))

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
        (hbl-append (t "((you need to opt in to write ") (rust-tt "unsafe") (t " code))")))))


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
       (rust-tt/nl #<<RUSTC_ERROR_MSG
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


