#lang racket
(require slideshow)
(provide outline rust-tt/nl tt/nl rust-tt recolorize)

(current-main-font "Open Sans")

(define (recolorize pict-thunk color)
  (colorize (parameterize ((black-and-white #t)) (pict-thunk)) color))

(define rust-keywords
  '("_" "fn" "let" "self" "as" "break" "const" "do" "else" "enum" "extern"
    "false" "fn" "for" "if" "impl" "let" "loop" "match" "mod" "mut" "once"
    "priv" "pub" "ref" "return" "static" "self" "struct" "super"
    "true" "trait" "type" "unsafe" "use" "while" "in" "continue" "proc"))

(define (concat op l)
  (cond ((null? (cdr l)) (car l))
        (else (op (car l) (concat op (cdr l))))))

(define (regexp-split-but-keep re string)
  (let ((pos* (regexp-match-positions* re string)))
    (let loop ((accum '()) (p pos*) (last 0))
      (cond ((null? p)
             (reverse (cons (substring string last (string-length string)) accum)))
            (else
             (let* ((m (car p))
                    (s (car m))
                    (e (cdr m)))
               (let ((prev (cons (substring string last s) accum)))
                 (loop (cons (substring string s e) prev) (cdr p) e))))))))

(define (rust-tt string)
  (define (one tok)
    (cond ((member tok rust-keywords)                      (colorize (tt tok) "dark green"))
          ((regexp-match #rx"^[a-zA-Z][a-zA-Z0-9]*!$" tok) (colorize (tt tok) "maroon"))
          ((regexp-match #rx"^[0-9][0-9_]*$" tok)          (colorize (tt tok) "blue"))
          (else                                            (tt tok))))
  (concat hbl-append
          (map one (regexp-split-but-keep #rx"[,.&<=>{};: ]|\\(|\\)" string))))

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

(define (rust-tt/nl string)
  (apply vl-append (map rust-tt (fragment-at-newlines string))))
