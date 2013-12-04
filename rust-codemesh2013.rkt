#lang racket
(require slideshow)
(require "rust-codemesh2013-common.rkt")
(require "rust-codemesh2013-sect1.rkt")
(require "rust-codemesh2013-sect2.rkt")
(require "rust-codemesh2013-sect3.rkt")
(require "rust-codemesh2013-sect4.rkt")
(require "rust-codemesh2013-sect5.rkt")

(string-append "Programming languages are in constant development, "
               "responding to the changing nature of computing "
               "problems and hardware infrastructure. ")

#;(begin 
  (set! actual-screen-w 800)
  (set! actual-screen-h 600))

(current-main-font "Open Sans")


; (slide (t "Hello World"))
; (slide (t "fn main() { println!(\"Hello World\"); }"))
; (slide (t/nl "fn main() {\n    println!(\"Hello World\");\n}"))

#;(current-slide-assembler (lambda (s v-sep c) c))
