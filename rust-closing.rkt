#lang racket
(require slideshow)
(require "rust-common.rkt")

(slide #:title "Topics not covered"
       (item "subtype relation of" (tt "&'a T") "and" (tt "&'b T") "induced by" (tt "'b:'a"))
       (item "traits as existentials (object-oriented dispatch)")
       (item "borrow-checking static analysis rules")
       (item "Rust and closures")
)

(slide #:title "Join the Fun!"
       (tt "rust-lang.org")
       (scale (bitmap "rust_logo-only_RGB.png") 0.1)
       (hbl-append (t "mailing-list: ") (tt "rust-dev@mozilla.org"))
       (hbl-append (t "community chat: ") (tt "irc.mozilla.org :: #rust"))
       (scale (bitmap "mozilla_wordmark_CS4.png") 0.1)
       )
