#lang racket
;(current-command-line-arguments '#("-s" "800" "600"))
(require slideshow)
(require "rust-meetup2014-common.rkt")
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
         (t "http://rust-lang.org/")
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

(let* ((c++-color    "dark green")
       (erlang-color "maroon")
       (sml-color    "blue")
       (mixed-color  "purple")
       (c++    (colorize (t "c++")    c++-color))
       (erlang (colorize (t "erlang") erlang-color))
       (sml    (colorize (t "sml")    sml-color)))

  (slide #:name "Rust is post-grad C++"
         (vr-append (para  "``rust is like" c++ "grew up and went to grad school,"
                           "shares an office with" erlang
                           ", and is dating" sml "''")
                    (text "-various, #rust"
                          (cons 'italic (current-main-font))
                          (current-font-size)))
         'next
         (colorize (para "stack allocation;"
                         "memory layout;"
                         "monomorphisation of generics")
                   c++-color)
         (colorize (para "safe task-based concurrency, failure") erlang-color)
         (colorize (para "type safety;"
                         (colorize (t "destructuring bind;") mixed-color)
                         "type classes")
                   sml-color)
         ))
;; A Web Browser is a multi-tasking operating system (e.g. page/tab is a task!)


(slide #:title "Motivation"
       ;(item "Why Mozilla is investing in a new programming language")
       (item "Why invest in a new programming language")
       'next
       (item "Web browsers are complex programs")
       (item "Expensive to innovate and compete while implementing atop standard systems languages")
       'next
       (item "So to implement next-gen browser, Servo ...")
       (subitem #:bullet (t "⇒") (tt "http://github.com/mozilla/servo"))
       'next
       (item "... Mozilla is using (& implementing) Rust")
       (subitem #:bullet (t "⇒") (tt "http://rust-lang.org")))

(outline 'one)

(slide #:title "The Rust Project"
       (item "Goal: bridge performance gap between safe and unsafe languages")
       (item "Design choices largely fell out of that requirement")
       (item "Rust compiler, stdlib, and tools are all MIT/Apache dual license.")
       (item "(also, very active community)"))

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

(slide #:title "Rust Objectives"
       (item "Sound Type checking")
       (subitem "Eschew runtime overhead in safe abstractions")
       'next
       (item "Can opt-in to unsafe code")
       (subitem "''Well-typed programs help assign blame.''")
       (subitem "plus, even safe code can fail (but in controlled fashion)")
       'next
       (item "Simple source ⇔ compiled code relationship")
       )
