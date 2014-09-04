#lang racket
(require slideshow)
(require "playpen.rkt")
(provide slide-code/tiny-url slide-code/url slide-code)
(provide slide-code/tiny-url/punchline slide-code/url/punchline)

(define (slide-code/tiny-url tiny-url title code-string)
  (call-with-url-and-code tiny-url code-string
                          (lambda (pu code)
                            (slide #:title title #:layout 'top pu (frame code)))))

(define (slide-code/url title code-string)
  (call-with-url-and-code code-string
                          (lambda (pu code)
                            (slide #:title title #:layout 'top pu (frame code)))))

(define (slide-code title code-string)
  (call-with-url-and-code code-string
                          (lambda (pu code)
                            (slide #:title title #:layout 'top (frame code)))))

(define (punchline-handler title punchline)
  (lambda (url code)
    (slide #:title title
           #:layout 'top
           url
           (frame code)
           'next
           punchline)))

(define (slide-code/url/punchline title code-string punchline)
  (call-with-url-and-code code-string
                          (punchline-handler title punchline)))

(define (slide-code/tiny-url/punchline tiny-url title code-string punchline)
  (call-with-url-and-code tiny-url code-string
                          (punchline-handler title punchline)))
