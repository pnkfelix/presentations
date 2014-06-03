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
(require "rust-meetup2014-common.rkt")

(slide #:title "Topics not covered"
       (item "regions/lifetimes and their subtyping relationship")
       ; (item "traits as existentials (object-oriented dispatch)")
       (item "borrow-checking static analysis rules")
       (item "freezing/thawing data structures")
       ; (item "task-local storage")
       ; (item "linked-failure")
       (item "one-shot closures: " (tt "proc"))
       ;(item "syntax extensions")
)

(slide #:name "Our community"
       (para "The Rust team: "
             "Brian Anderson, "
             "Nick Cameron, "
             "Alex Chrichton, "
             "Felix Klock (me), "
             "Niko Matsakis, "
             "Patrick Walton")
       (para "Interns/Alumni: "
             "Graydon Hoare, "
             "Michael Bebenita," "Ben Blum,"
             "Tim Chevalier,"
             "Rafael Espíndola,"
             "Roy Frostig, " "Marijn Haverbeke, " "Eric Holk, "
             "Lindsey Kuper, " "Elliott Slaughter, " "Paul Stansifer, "
             "Michael Sullivan")
       (t "(and the many members of the larger Rust community)")
       (t "")
       (t "http://rust-lang.org/"))

(slide #:title "Join the Fun!"
       (tt "rust-lang.org")
       (scale (bitmap "rust_logo-only_RGB.png") 0.1)
       (hbl-append (t "mailing-list: ") (tt "rust-dev@mozilla.org"))
       (hbl-append (t "community chat: ") (tt "irc.mozilla.org :: #rust"))
       (scale (bitmap "mozilla_wordmark_CS4.png") 0.1)
       )
