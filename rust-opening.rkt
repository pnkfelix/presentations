#lang racket

(require slideshow)
(require "rust-common.rkt")
(require (only-in racket/draw make-color))

(define (haiku line1 line2 line3 text-style text-size . rest)
  (let ((author-info
         (if (null? rest)
             '()
             (let ((author (list-ref rest 0))
                   (author-style (list-ref rest 1))
                   (author-size (list-ref rest 2)))
               (list (text (string-append " -" author)
                           author-style author-size))))))
    (apply hbl-append 20
           (vc-append (text line1 text-style text-size)
                      (text line2 text-style text-size)
                      (text line3 text-style text-size))
           author-info)))

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
                   ;; "lkuper" (cons 'italic base-style) 24
                   ))
       (thw (pict-width the-haiku))
       (thh (pict-height the-haiku)))

  (slide rust-logo
         (pin-over moz-logo (* 2/3 rw) (- thh) the-haiku)))
