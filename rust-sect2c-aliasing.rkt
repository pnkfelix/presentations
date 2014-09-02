#lang racket
(require slideshow)
(require "slides-common.rkt")
(require "rust-common.rkt")
(require "playpen.rkt")

(define (slide-code/tiny-url tiny-url title code-string)
  (call-with-url-and-code tiny-url code-string
                          (lambda (pu code)
                            (slide #:title title #:layout 'top pu code))))

(define (slide-code/url title code-string)
  (call-with-url-and-code code-string
                          (lambda (pu code)
                            (slide #:title title #:layout 'top pu code))))

(define (slide-code title code-string)
  (call-with-url-and-code code-string
                          (lambda (pu code)
                            (slide #:title title #:layout 'top code))))

(slide #:name "Why all the fuss about aliasing?"
       'alts
       (list (list (big-t "Why all the fuss about aliasing?"))
             (list (big-t "It is for type soundness"))))

(define (aliasing-example call-site-color assign-to-color)
  (define (val&color-val vc) (if (pair? vc) (car vc) vc))
  (define (val&color-color vc) (if (pair? vc) (cadr vc) ""))
  (let* ((call-site (val&color-val call-site-color))
         (assign-to (val&color-val assign-to-color))
         (color-call (val&color-color call-site-color))
         (color-assign (val&color-color assign-to-color))
         (template #<<RUST
fn main() {
    // SLIDE START
    fn add3(x:int) -> int { x + 3 }
    enum E { A(fn (int) -> int), B(int) }
    let mut a = A(add3); let mut b = B(17);
    let p1 = &mut a;     let p2 = &mut b;
    $$CALL_SITE$$ $$COLOR_CALL$$

    fn foo(p1: &mut E, p2: &mut E) {
        match p1 {
            &B(..) => fail!("cannot happen"),
            &A(ref adder) => {
                $$ASSIGN_TO$$ = B(0xdeadc0de); $$COLOR_ASSIGN$$
                println!("{}", (*adder)(14));
            }
        }
    }
    // SLIDE FINIS
}
RUST
)
         (template (string-replace template "$$ASSIGN_TO$$" assign-to))
         (template (string-replace template "$$CALL_SITE$$" call-site))
         (template (string-replace template "$$COLOR_ASSIGN$$" color-assign))
         (template (string-replace template "$$COLOR_CALL$$" color-call)))
    template))

(call-with-url-and-code
 (aliasing-example "foo(p1, p2);" "/* watch: */ *p2")
 (lambda (url code)
   (slide #:title "mutable aliasing ⇒ soundness holes"
          ;; url
          (frame code)
          'next
          (item "(punchline: above is fine;" (tt "rustc") "accepts it)")
          )))

(call-with-url-and-code
 (aliasing-example "foo(p1, p2);" "/* was p2 */ *p1")
 (lambda (url code)
   (slide #:title "mutable aliasing ⇒ soundness holes"
          ;; url
          (frame code)
          )))

(call-with-url-and-code
 (aliasing-example "foo(p1, p2);" '("/* was p2 */ *p1" "// COLOR:red"))
 (lambda (url code)
   (slide #:title "mutable aliasing ⇒ soundness holes"
          ;; url
          (frame code)
          'next
          (item "(punchline: above is badness;" (tt "rustc") "rejects it)")
          )))

(call-with-url-and-code
 (aliasing-example "foo(p1, p2);" "/* watch? */ *p2")
 (lambda (url code)
   (slide #:title "mutable aliasing ⇒ soundness holes"
          ;; url
          (frame code)
          (item "(reminder: above is fine;" (tt "rustc") "accepts it)")
          )))

(call-with-url-and-code
 (aliasing-example "foo(p1, p1);" "/* watch? */ *p2")
 (lambda (url code)
   (slide #:title "mutable aliasing ⇒ soundness holes"
          ;; url
          (frame code)
          )))

(call-with-url-and-code
 (aliasing-example '("foo(p1, p1);" "// COLOR:red") "/* watch? */ *p2")
 (lambda (url code)
   (slide #:title "mutable aliasing ⇒ soundness holes"
          ;; url
          (frame code)
          'next
          (item "(punchline: above is badness;" (tt "rustc") "rejects it)")
          )))

(slide #:name "Why all the fuss about move semantics revisited?"
       'alts
       (list (list (big-t "Why all the fuss about move semantics?"))
             (list (big-t "Allows us to reason about aliasing"))))
