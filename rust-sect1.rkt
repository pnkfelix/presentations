#lang racket

(require slideshow)
(require "rust-common.rkt")

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

       (item (let ((orig (t "''Well-typed programs can't go wrong.''")))
               (pin-balloon (wrap-balloon (text "Milner, 1978" null 16)
                                          'sw 0 0) orig (pict-width orig) 0)))
       (comment "Robin Milner, ''A theory of type polymorphism in programming'' 1978")
       'next
       (item "More generally: identify classes of errors ...")
       (subitem "... then use type system to remove them")
       (subitem "(or at least isolate them)")
       (item "Eases reasoning; adds confidence")
       'next
       (item (let ((orig (t "Well-typed programs help assign blame.")))
               (pin-balloon (wrap-balloon (apply vl-append
                                                 (map (lambda (t) (text t null 16))
                                                      '("Tobin-Hochstadt 2006," "Wadler 2009")))
                                          'sw 0 0)
                            orig (pict-width orig) 0)))
       (subitem "(" (tt "unsafe") "code can still ``go wrong'')")
       (subitem "and even safe code can" (tt "fail")
                                        ;"(but only in controlled fashion)")
                )
       )

(slide #:title "Simple source ⇔ compiled code relationship"
       (item "A reason C persists to this day")
       (item "Programmer can mentally model machine state")
       (comment "especially with respect to memory")
       'next
       (subitem "can also control low-level details (e.g. memory layout)")
       'next
       (item "Goal for Rust: preserve this relationship ...")
       'next
       (subitem "... while" (bt "retaining") "memory safety ...")
       (comment "One definition of memory safety: programs can only dereference"
                "pointers to previously allocated memory that has not yet been"
                "freed.")
       'next
       (subitem "... without runtime cost.")
       (comment "In languages like Java/Haskell/ML, safe abstractions have runtime costs:"
                "boxing everything; garbage-collecting everything")
       (subitem "Do not box everything; do not GC-manage everything.")
       )
