#lang racket

;; Copyright 2014 Felix S. Klock II. See the COPYRIGHT
;; file at the top-level directory of this distribution.
;;
;; Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
;; http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
;; <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
;; option. This file may not be copied, modified, or distributed
;; except according to those terms.

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

; (slide (t "Hello World"))
; (slide (t "fn main() { println!(\"Hello World\"); }"))
; (slide (t/nl "fn main() {\n    println!(\"Hello World\");\n}"))

#;(current-slide-assembler (lambda (s v-sep c) c))
