#lang racket

(require slideshow)
(require "rust-common.rkt")

(require (only-in browser/external send-url))

(require slideshow/balloon)

(define (quiz-code fill)
  (let ((template #<<RUST_SIZEOF
#[deriving(Show)] struct S1 { x: i32 }
#[deriving(Show)] struct S2 { y: i32 }

$$IMPL$$

fn main() {
  use std::mem::size_of;
  println!("S1 {:u} bytes, S2 {:u} bytes",
           size_of::<S1>(), size_of::<S2>());
  assert_eq!(size_of::<S1>(), size_of::<S2>());
}
RUST_SIZEOF
  ))
    (string-replace template "$$IMPL$$" fill)
    ))

(define playpen-url
  (case-lambda
    ((show-url url)
     (clickback (let ((pp (colorize (tt "playpen ") "orange")))
                  (if show-url
                      (hb-append pp
                                 (tt "(")
                                 (colorize (tt show-url) "blue")
                                 (tt ")"))
                      pp))
                (λ () (send-url url))))
    ((url)
     (playpen-url url url))))

(define (encode-playpen-url code-string)
  (let* ((prefix "http://play.rust-lang.org/?code=")
         (encoded (foldl (lambda (replace s)
                           (string-replace s (car replace) (cadr replace)))
                         code-string
                         '(("%" "%25") ;; must be 1st (avoid double-replace)
                           ("$" "%24")
                           ("&" "%26")
                           
                           ("+" "%2B")
                           ("," "%2C")
                           ("/" "%2F")
                           (":" "%3A")
                           (";" "%3B")
                           ("=" "%3D")
                           ("?" "%3F")
                           ("@" "%40")
                           ("\"" "%22")
                           ("|" "%7C")
                           ("\\" "%5C")
                           ("^" "%5E")
                           ("~" "%7E")
                           ("`" "%60")
                           
                           ("{" "%7B")
                           ("}" "%7D")
                           ("<" "%3C")
                           (">" "%3E")
                           ("[" "%5B")
                           ("]" "%5D")
                           ("#" "%23")
                          
                           )))
         (u (string-append prefix encoded)))
    ;; (display u)
    ;; (newline)
    u))

(define (extract-code-to-present code-string)
  (if (not (regexp-match #rx"// SLIDE" code-string))
      code-string
      (let* ((input code-string)
             (input (cond ((regexp-match #rx"// *SLIDE START(.*)" input) => cadr)
                          (else input)))
             (input (cond ((regexp-match #rx"(.*)// *SLIDE FINIS" input) => cadr)
                          (else input)))
             (input (regexp-replace* #rx"\n([^\n]*// *SLIDE HIDE)" input "")))
        input)))

(define (slide-code/url title code-string)
  (slide #:title title
         #:layout 'top
         (playpen-url #f (encode-playpen-url code-string))
         (rust-tt/nl (extract-code-to-present code-string))))

(define (slide-code title code-string)
  (slide #:title title
         #:layout 'top
         (rust-tt/nl (extract-code-to-present code-string))))

  
;(send-url (encode-playpen-url "fn main() { println!(\"Hello world\"); }"))

(begin
(slide-code/url "Pop Quiz"
                (quiz-code  #<<TEMPLATE
/* 
 *
 * (various impls elided)
 *
 */
TEMPLATE
                                   ))

(slide-code/url "The Joke"
                (quiz-code #<<DROP_CODE
impl Drop for S2 {
  fn drop(&mut self) {
    println!("Hi for {}", self);
  }
}
DROP_CODE
                                   ))


(slide-code/url "Why"
                #<<DYN_DROP_SEMANTICS
#![feature(macro_rules)]
#![allow(unused_variable)]
use std::fmt;

#[deriving(Clone,Show)] struct S { name: &'static str }

#[deriving(Clone,Show)] struct Df { name: &'static str }

#[deriving(Clone,Show)] struct Pair<X,Y>{ x: X, y: Y }

static mut current_indent: uint = 0;

fn push_indent() {
    unsafe { current_indent += 4; }
}

fn pop_indent() {
    unsafe { current_indent -= 4; }
}

struct Indent;
impl Indent {
    fn more() -> Indent { push_indent(); Indent }
}
impl Drop for Indent {
  fn drop(&mut self) { pop_indent(); }
}

fn indent() -> String {
    String::from_char(unsafe { current_indent }, ' ')
}

impl Drop for Df {
    fn drop(&mut self) {
        println!("{}dropping Df {}", indent(), self.name)
    }
}

fn take_and_pass<T:fmt::Show>(t: T) {
    println!("{}t-n-p took and will pass: {}", indent(), &t);
    push_indent();
    take_and_drop(t);
    pop_indent();
}

fn take_and_drop<T:fmt::Show>(t: T) {
    println!("{}t-n-d took and will drop: {}", indent(), &t);
}

// SLIDE START
fn foo(b: || -> bool) {
    let f3 = Df  { name: "f3" };
    let f4 = Df  { name: "f4" };
    let f5 = Df  { name: "f5" };
    let p = Pair { x: f4, y: f5 };
    let _f10 = Df { name: "f10" };

    push_indent(); // SLIDE HIDE
    if b() {
        // `p.x` consumed by `take_and_pass`
        take_and_pass(p.x);
    }
    // drops here (f10,[ f4,] f5, f3)
    pop_indent(); // SLIDE HIDE
}
// SLIDE FINISH
// At end of foo, we drop each local variable,
// in reverse order of declaration.
// So when `b` is `|| true`, we should see the following drop sequence:
// drop(_f10), printing "Df f10"
// drop(p)
//   ==> drop(p.y), printing "Df f5"
//   ==> attempt to drop(and skip) already-dropped p.x, no-op
// no drop of `f5` since it was moved into `p`
// no drop of `f4` since it was moved into `p`
// drop(f3), printing "f3"


fn main() {
    println!("Running `foo(|| true)`");
    {
      let i = Indent::more();
      foo(|| true);
    }
    println!("Running `foo(|| false)`");
    {
      let i = Indent::more();
      foo(|| false);
    }
}
DYN_DROP_SEMANTICS
                )
)

(slide-code "The Fix: Static Drop Semantics"
            #<<STATIC_DROP_SEMANTICS
fn foo(b: || -> bool) {  // DROP OBLIGATIONS
  let f3 = Df { ... };   // { f3 }
  let f4 = Df { ... };   // { f3,f4 }
  let f5 = Df { ... };   // { f3,f4,f5 }
  let p = Pair{ x: f4, 
                y: f5 }; // { f3,     , p }
  let _f10 = Df { ... }; // { f3,     , p,   _f10 }
  if b() {
                         // { f3,     , p,   _f10 }
    take_and_pass(p.x);
                         // { f3,     , p.y, _f10 }
  } else {
                         // { f3,     , p,   _f10 }
  }
  // drop mismatch here (p versus p.y)
}
STATIC_DROP_SEMANTICS
            )


(slide ; #:title "Flowgraph Based"
 (vl-append (tt "rustc ex.rs --pretty flowgraph=foo \\")
            (tt "            -Z flowgraph-print-needs-drop"))
 (ht-append (bitmap "tiny_graph.png")
            (bitmap "zoom_graph.png"))       
 )

(slide-code "Common Case: Auto-Inserted Drop"
            #<<STATIC_DROP_SEMANTICS
fn foo(b: || -> bool) {  // DROP OBLIGATIONS
  let f3 = Df { ... };   // { f3 }
  let f4 = Df { ... };   // { f3,f4 }
  let f5 = Df { ... };   // { f3,f4,f5 }
  let p = Pair{ x: f4, 
                y: f5 }; // { f3,     , p }
  let _f10 = Df { ... }; // { f3,     , p,   _f10 }
  if b() {
                         // { f3,     , p,   _f10 }
    take_and_pass(p.x);
                         // { f3,     , p.y, _f10 }
  } else {
                         // { f3,     , p,   _f10 }
    rustc_inserted_drop(p.x); // COLOR:red
                         // { f3,     , p.y, _f10 }
  }
}
STATIC_DROP_SEMANTICS
            )

(slide
 #:title "Gotcha"
 (item "Drops with side-effects?")
 'next
 (item "RAII patterns")
 'next
 (subitem "locks")
 (subitem "flushing buffers")
 (subitem "etc (?)"))

(define lint-demo
  (lambda (message)
    (string-replace #<<LINT_DEMO
  let p = Pair{ x: f4, 
                y: f5 }; // { f3,     , p }
  let _f10 = Df { ... }; // { f3,     , p,   _f10 }
  if b() {
                         // { f3,     , p,   _f10 }
    take_and_pass(p.x);
                         // { f3,     , p.y, _f10 }
  } else {
                         // { f3,     , p,   _f10 }
$$MESSAGE$$
  }
  other_code();
}
LINT_DEMO
                    "$$MESSAGE$$" message
    )))


(slide #:title "Lint to the Rescue"
       #:layout 'top
       'alts
       (map (lambda (message)
              (list 
              (rust-tt/nl (extract-code-to-present
                           (lint-demo message)))))
            (list #<<NO_MESSAGE




NO_MESSAGE
                  #<<RUSTC_WARNING
    warning: `p.x` initialized here, but not on // COLOR:orange
    other paths. (Use Option, call `drop()`, or // COLOR:orange
    reinitialize elsewhere.)                    // COLOR:orange
    #[warn(unmarked_early_drop)] on by default  // COLOR:orange
RUSTC_WARNING
                  )))

(slide-code "Tiers of linting"
            #<<EXAMPLE_OF_MARKED_DROP
#[lang="noisy_drop"] trait NoisyDrop { }       
#[lang="quiet_drop"] trait QuietDrop { }

              // Drops default to quiet
impl Drop for Df { ... }
              // ...until marked noisy.
impl NoisyDrop for Df {}

              // Noisiness bubbles out...
struct S { d: Df }
struct Q { s: S }

              // ... until marked quiet again.
impl QuietDrop for Q {}

#[warn(early_loud_drop)]
#[allow(early_quiet_drop)]
EXAMPLE_OF_MARKED_DROP
            )

(slide
 (t "Thanks for listening"))