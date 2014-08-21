#lang racket

(require slideshow)
(require "rust-common.rkt")

(require (only-in browser/external send-url))

(require slideshow/balloon)

(define quiz-code
  #<<RUST_SIZEOF
#[deriving(Show)] struct S1 { x: i32 }
#[deriving(Show)] struct S2 { y: i32 }

$$IMPLS$$

fn main() {
  use std::mem::size_of;
  println!("S1 {:u} bytes, S2 {:u} bytes",
           size_of::<S1>(), size_of::<S2>());
  assert_eq!(size_of::<S1>(), size_of::<S2>());
}
RUST_SIZEOF
  )

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

(define (slide-code/url title code-string)
  (slide #:title title
         #:layout 'top
         (playpen-url #f (encode-playpen-url code-string))
         (rust-tt/nl code-string)))
  
;(send-url (encode-playpen-url "fn main() { println!(\"Hello world\"); }"))

#;
(begin
(slide-code/url "A Quiz"
                (string-replace quiz-code "$$IMPLS$$" #<<TEMPLATE
/* 
 *
 * (various impls elided)
 *
 */
TEMPLATE
                                   ))

(slide-code/url "The Joke"
                (string-replace quiz-code "$$IMPLS$$" #<<DROP_CODE
impl Drop for S2 {
  fn drop(&mut self) {
    println!("Hi for {}", self);
  }
}
DROP_CODE
                                   ))
)

(slide-code/url "Why"
                #<<DYN_DROP_SEMANTICS
fn main() {
  println!("Hello world");
}
DYN_DROP_SEMANTICS
                )