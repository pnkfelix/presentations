#lang racket
(require slideshow)
(require "slides-common.rkt")
(require "rust-common.rkt")
(require "playpen.rkt")
(require "slide-rust-code.rkt")

(slide #:name "Why all the fuss about aliasing?"
       'alts
       (list (list (big-t "Why all the fuss about aliasing?"))
             (list (big-t "It is for type soundness"))))

(define (aliasing-example call-site-color assignment-color)
  (define (val&color-val vc) (if (pair? vc) (car vc) vc))
  (define (val&color-color vc) (if (pair? vc) (cadr vc) ""))
  (let* ((call-site (val&color-val call-site-color))
         (assignment (val&color-val assignment-color))
         (color-call (val&color-color call-site-color))
         (color-assign (val&color-color assignment-color))
         (template #<<RUST
fn main() {
    // SLIDE START
    enum E { A(fn (int) -> int), B(int) }
    fn add3(x:int) -> int { x + 3 }
    let mut a = A(add3);        let mut b = B(17);
    let m1 = &mut a;            let m2 = &mut b;
    $$CALL_SITE$$ $$COLOR_CALL$$

    fn foo(p1: &mut E, p2: &mut E) {
        match p1 {
            &B(..) => fail!("cannot happen"),
            &A(ref adder) => {
                $$ASSIGN$$ $$COLOR_ASSIGN$$
                println!("{}", (*adder)(14));
    } } }
    // SLIDE FINIS
}
RUST
)
         (template (string-replace template "$$ASSIGN$$" assignment))
         (template (string-replace template "$$CALL_SITE$$" call-site))
         (template (string-replace template "$$COLOR_ASSIGN$$" color-assign))
         (template (string-replace template "$$COLOR_CALL$$" color-call)))
    template))

(call-with-url-and-code
 (aliasing-example "foo(m1, m2);" "/* WATCH: */ *p2 = B(0xBadC0de);")
 (lambda (url code)
   (slide #:title "mutable aliasing ⇒ soundness holes"
          #:layout 'top
          url
          (frame code)
          'next
          (item "(punchline: above is fine;" (tt "rustc") "accepts it)")
          )))

(call-with-url-and-code
 (aliasing-example "foo(m1, m2);" "/* was p2 */ *p1 = B(0xBadC0de);")
 (lambda (url code)
   (slide #:title "mutable aliasing ⇒ soundness holes"
          #:layout 'top
          url
          (frame code)
          )))

(call-with-url-and-code
 (aliasing-example "foo(m1, m2);" '("/* was p2 */ *p1 = B(0xBadC0de);" "// COLOR:red"))
 (lambda (url code)
   (slide #:title "mutable aliasing ⇒ soundness holes"
          #:layout 'top
          url
          (frame code)
          'next
          (item "(punchline: above is badness;" (tt "rustc") "rejects it)")
          )))

(call-with-url-and-code
 (aliasing-example "foo(m1, m2);" "unsafe { *(p1 as *mut E)=B(7); }")
 (lambda (url code)
   (slide #:title "mutable aliasing ⇒ soundness holes"
          #:layout 'top
          url
          (frame code)
          )))

(call-with-url-and-code
 (aliasing-example "foo(m1, m2);" "unsafe { *(p1 as *mut E)=B(7); } // COLOR:red")
 (lambda (url code)
   (slide #:title "mutable aliasing ⇒ soundness holes"
          #:layout 'top
          url
          (frame code)
          'next
          (item "Emphasis:" (rust-tt "unsafe") "means \"can crash.\"")
          )))

(call-with-url-and-code
 (aliasing-example "foo(m1, m2);" "/* watch? */ *p2 = B(0xBadC0de);")
 (lambda (url code)
   (slide #:title "mutable aliasing ⇒ soundness holes"
          #:layout 'top
          url
          (frame code)
          (item "(reminder: above is fine;" (tt "rustc") "accepts it)")
          )))

(call-with-url-and-code
 (aliasing-example "foo(m1, m1);" "/* watch? */ *p2 = B(0xBadC0de);")
 (lambda (url code)
   (slide #:title "mutable aliasing ⇒ soundness holes"
          #:layout 'top
          url
          (frame code)
          (item "what changed, nothing in" (tt "foo") "..."))
          ))

(call-with-url-and-code
 (aliasing-example '("foo(m1, m1);" "// <~~ AHHHHH // COLOR:red")
                   "/* watch? */ *p2 = B(0xBadC0de);")
 (lambda (url code)
   (slide #:title "mutable aliasing ⇒ soundness holes"
          #:layout 'top
          url
          (frame code)
          'next
          (item "(punchline: above is badness;" (tt "rustc") "rejects it)")
          )))

(slide #:name "Why all the fuss about move semantics revisited?"
       'alts
       (list (list (big-t "Why all the fuss about move semantics?"))
             (list (big-t "Allows us to reason about aliasing"))))
