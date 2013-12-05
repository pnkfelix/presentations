#lang racket
(require slideshow)
(provide outline tt/nl)

(current-main-font "Open Sans")

(define outline
  (let ((sub-para (lambda l (para #:width (* 3/4 (current-para-width)) l))))
    (make-outline
     'one   "Part I: Motivation"
     (lambda (tag) (sub-para "Why Mozilla is investing in Rust"))

     'two   "Part II: Rust syntax and semantics"
     (lambda (tag)
       (sub-para "Systems programming under the influence of FP")
       #;(sub-para "Standing on the shoulders of giants"))

     'three "Part III: Ownership and borrowing"
     (lambda (tag) (sub-para "How Rust handles pointers"))

     'four  "Part IV: Concurrency model"
     (lambda (tag) (sub-para "Communication between tasks")))))


;; string -> [listof string)
(define (fragment-at-newlines str)
  ;; [listof char] [listof char] -> [listof [listof char]]
  (define (fragment accum chars)
    (cond ((null? chars) 
           (cond ((null? accum) '())
                 (else (list (reverse accum)))))
          (else 
           (cond ((char=? (car chars) #\newline)
                  (cons (reverse accum) (fragment '() (cdr chars))))
                 (else
                  (fragment (cons (car chars) accum) (cdr chars)))))))
  (map list->string (fragment '() (string->list str))))

(define (tt/nl string)
  (apply vl-append (map tt (fragment-at-newlines string))))
