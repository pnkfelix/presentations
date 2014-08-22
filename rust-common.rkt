#lang racket
(require slideshow)
(provide outline rust-tt/nl tt/nl rust-tt recolorize)
(provide ocaml-tt ocaml-tt/nl)

(current-main-font "Open Sans")

(define (recolorize pict-thunk color)
  (colorize (parameterize ((black-and-white #t)) (pict-thunk)) color))

(define rust-keywords
  '("_" "fn" "let" "self" "as" "break" "const" "do" "else" "enum" "extern"
    "false" "fn" "for" "if" "impl" "let" "loop" "match" "mod" "mut" "once"
    "priv" "pub" "ref" "return" "static" "self" "struct" "super"
    "true" "trait" "type" "unsafe" "use" "while" "in" "continue" "proc"))

(define ocaml-keywords
  '("and" "as" "assert" "begin" "class" "constraint"
    "do" "done" "downto" "else" "end" "exception" "external"
    "false" "for" "fun" "function" "functor"
    "if" "in" "include" "inherit" "inherit!" "initializer"
    "lazy" "let" "match" "method" "method!" "module" "mutable"
    "new" "object" "of" "open" "or" "private" "rec"
    "sig" "struct" "then" "to" "true" "try" "type"
    "val" "val!" "virtual" "when" "while" "with"))

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

(define (make-lang-tt lang-keywords)
  (lambda (line)
    (define (one tok)
      (cond ((member tok lang-keywords)                      (colorize (tt tok) "dark green"))
            ((regexp-match #rx"^[a-zA-Z][a-zA-Z0-9]*!$" tok) (colorize (tt tok) "maroon"))
            ((regexp-match #rx"^[0-9][0-9_]*$" tok)          (colorize (tt tok) "blue"))
            (else                                            (tt tok))))
    (let* ((recolor
            (cond ((regexp-match #rx"// COLOR:([^ ]*)" line) => cadr)
                  (else #f)))
           (line (regexp-replace* #rx"// COLOR:([^ ]*)" line ""))
           (words (map one (regexp-split-but-keep #rx"[,.&<=>{};: ]|\\(|\\)" line)))
           (line (concat hbl-append words))
           (line (if recolor (colorize line recolor) line)))
      line)))


(define rust-tt (make-lang-tt rust-keywords))

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
     )))


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


(define ocaml-tt (make-lang-tt ocaml-keywords))

(define (ocaml-tt/nl string)
  (apply vl-append (map ocaml-tt (fragment-at-newlines string))))
