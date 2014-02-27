#lang racket
(require slideshow)
(require "rust-meetup2014-common.rkt")
(require "rust-meetup2014-sect1.rkt")
(require "rust-meetup2014-sect2.rkt")
(require "rust-meetup2014-sect4.rkt")

(string-append "Programming languages are in constant development, "
               "responding to the changing nature of computing "
               "problems and hardware infrastructure. ")

#;(begin 
  (set! actual-screen-w 800)
  (set! actual-screen-h 600))

; (slide (t "Hello World"))
; (slide (t "fn main() { println!(\"Hello World\"); }"))
; (slide (t/nl "fn main() {\n    println!(\"Hello World\");\n}"))

#;(current-slide-assembler (lambda (s v-sep c) c))
