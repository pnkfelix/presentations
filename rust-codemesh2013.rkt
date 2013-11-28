#lang racket
;#lang slideshow
(require (lib "slideshow"))

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

(current-main-font "Open Sans")

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

(slide #:title "Tool: Sound Type Checking"
       (item "``Well-typed programs don't go wrong.''")
       'next
       (item "Revised: Well-typed programs can assist in blame assignment.")
       )

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
  

(define cake-c++-names (map car cake-c++-example))

(define (c++-code-for example name-of-part)
  (cadr (assq name-of-part example)))

(define (cake-c++-code-for name)
  (c++-code-for cake-c++-example name))

(define (c++-code-tt example specs)
  (letrec ((from-name (lambda (name) (tt/nl (c++-code-for example name))))
           (from-spec (lambda (spec)
                        (cond ((symbol? spec) (from-name spec))
                              ((eq? 'ghost (car spec)) (ghost (from-spec (cadr spec))))))))
    (apply vl-append gap-size (map from-spec specs))))


(define (c++-code-slide example specs)
  ;(slide #:layout 'top (c++-code-tt example specs))
  (slide (lt-superimpose (c++-code-tt example specs) full-page))
  )

(define (big-t s)
  (text s (current-main-font) 48))

(slide 'alts
       (list (list (big-t "An example"))
             (list (big-t "A (contrived, strawman) example"))))
       
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
 (t "We want Rust to make it harder to make silly mistakes.")
 'next
 (t "(but not impossible)")
 'next
 'alts
 (list (list
        (hbl-append (t "((you need to opt in to write unsafe code))")))
       (list
        (hbl-append (t "((you need to opt in to write ") (tt "unsafe") (t " code))")))))

#;(outline 'one)

#;(slide (tt/nl #<<HERE
fn main() {
   println!("Hello world")
}
HERE
             ))
