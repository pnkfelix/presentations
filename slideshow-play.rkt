#lang slideshow

;; Copyright 2014 Felix S. Klock II. See the COPYRIGHT
;; file at the top-level directory of this distribution.
;;
;; Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
;; http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
;; <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
;; option. This file may not be copied, modified, or distributed
;; except according to those terms.

(define blue-fish (standard-fish (* 3 gap-size) (* 2 gap-size) 
                                 #:direction 'right 
                                 #:color "blue" 
                                 #:eye-color "white"))

(define (encircled p)
  (let* ((w (pict-width p))
         (h (pict-height p))
         (d (max w h))
         (d (* d (sqrt 2)))
         (c (circle d)))
    (cc-superimpose c p)))

(define (bound-frame p)
  (frame p #:color "green"))

(define plain-file (file-icon (* 2 gap-size) (* 3 gap-size) #t))

(define encircled-blue-fish (encircled blue-fish))
(define encircled-plain-file (encircled plain-file))

(define fish-file-scene (bound-frame 
                         (inset (ht-append (* 4 gap-size) 
                                           blue-fish 
                                           (inset plain-file 0 (* 2 gap-size) 0 0))
                                gap-size)))


(define fish-file-scene2 (bound-frame 
                         (inset (ht-append (* 4 gap-size) 
                                           encircled-blue-fish 
                                           encircled-blue-fish
                                           (inset encircled-plain-file 0 (* 4 gap-size) 0 0))
                                gap-size)))


(let-values ([(fdx fdy) (rc-find fish-file-scene blue-fish)]
              [(adx ady) (lt-find fish-file-scene plain-file)])
   (pin-over fish-file-scene
             fdx fdy
             (colorize
              (pip-arrow-line (- adx fdx) (- ady fdy) gap-size)
              "orange")))

(let-values ([(fdx fdy) (rc-find fish-file-scene2 encircled-blue-fish)]
              [(adx ady) (lt-find fish-file-scene2 encircled-plain-file)])
   (pin-over fish-file-scene2
             fdx fdy
             (colorize
              (pip-arrow-line (- adx fdx) (- ady fdy) gap-size)
              "orange")))
