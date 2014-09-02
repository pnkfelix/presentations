#lang racket
(require slideshow)
(provide big-t)

(define (big-t s)
  (text s (current-main-font) 48))
