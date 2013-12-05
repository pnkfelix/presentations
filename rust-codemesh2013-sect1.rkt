#lang racket
;(current-command-line-arguments '#("-s" "800" "600"))
(require slideshow)
(require "rust-codemesh2013-common.rkt")
(require (only-in racket/draw make-color))

(define (haiku line1 line2 line3 text-style text-size author author-style author-size)
  (hbl-append 20
              (vc-append (text line1 text-style text-size)
                         (text line2 text-style text-size)
                         (text line3 text-style text-size))
              (text (string-append " -" author) author-style author-size)))

(let* ((rust-logo (scale (bitmap "rust_logo_snip_RGB.png") 0.4))
       (rw (pict-width rust-logo))
       (rh (pict-height rust-logo))
       (base-style (cons (make-color 128 128 128)
                         (current-main-font)))
       (moz-logo (scale (bitmap "mozilla_wordmark_snip.png") 0.2))
       (mw (pict-width moz-logo))
       (mh (pict-width moz-logo))
       (the-haiku (haiku "a systems language"
                  "pursuing the trifecta"
                   "safe, concurrent, fast"
                   base-style 24
                   "lkuper"
                   (cons 'italic base-style) 24))
       (thw (pict-width the-haiku))
       (thh (pict-height the-haiku)))

  (slide rust-logo 
         (pin-over moz-logo (* 2/3 rw) (- thh) the-haiku)))

 ;; TODO: Overview of what Rust offers (one slide), then transition to the "Why"

;; Lindsey Kuper haiku:
;; a systems language
;; pursuing the trifecta
;; safe, concurrent, fast

;; Rust: dherman "Love-child of C++ Erlang and O'Caml/Haskell"
;; My preferred: C++ grew up, went to university, and was a roommate of Erlang and ML/Haskell/...

;; Stack allocation, data ownership, monomorphisation and inlining
;; Actors, message-passing, failure
;; (Goal of) type safety, pattern matching + destructuring, type clases, no null

;; High-level features of Rust of interest:
;; - type inference
;; - safe task-based concurrency
;; - higher-order functions
;; - pattern matching and algebraic data types
;; - polymorphism

;; A Web Browser is a multi-tasking operating system (e.g. page/tab is a task!)


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

;; XXX FIXME read the article, be able to explain this

(slide #:title "Zero-cost abstractions"
       (comment "Cite Robert O'Callahan if possible")
       (item "Goal: do not pay at runtime for a feature unused by program")
       (item "There is still a non-zero cognitive cost")
       (subitem "Often must think more about data representation")
       (subitem "Make choices about memory allocation")
       (item "But in safe blocks of code, compiler checks our assumptions"))
