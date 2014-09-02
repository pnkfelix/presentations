#lang racket
(require slideshow)
(require (only-in browser/external send-url))
(require "rust-common.rkt")
(provide call-with-url-and-code)
;(provide slide-code slide-code/url slide-code/tiny-url)

(define playpen-url
  (case-lambda
    ((show-url url)
     (clickback (let ((pp (colorize (tt "playpen ") "orange")))
                  (if show-url
                      (hb-append pp
                                 (tt "(")
                                 (colorize (tt show-url) "blue")
                                 (tt ")"))
                      pp))
                (λ () (send-url url))))
    ((url)
     (playpen-url url url))))

(define (encode-playpen-url code-string)
  (let* ((prefix "http://play.rust-lang.org/?code=")
         (encoded (foldl (lambda (replace s)
                           (string-replace s (car replace) (cadr replace)))
                         code-string
                         '(("%" "%25") ;; must be 1st (avoid double-replace)
                           ("$" "%24")
                           ("&" "%26")

                           ("+" "%2B")
                           ("," "%2C")
                           ("/" "%2F")
                           (":" "%3A")
                           (";" "%3B")
                           ("=" "%3D")
                           ("?" "%3F")
                           ("@" "%40")
                           ("\"" "%22")
                           ("|" "%7C")
                           ("\\" "%5C")
                           ("^" "%5E")
                           ("~" "%7E")
                           ("`" "%60")

                           ("{" "%7B")
                           ("}" "%7D")
                           ("<" "%3C")
                           (">" "%3E")
                           ("[" "%5B")
                           ("]" "%5D")
                           ("#" "%23")

                           )))
         (u (string-append prefix encoded)))
    ;; (display u)
    ;; (newline)
    u))

(define (extract-code-to-present code-string)
  (if (not (regexp-match #rx"// *SLIDE" code-string))
      code-string
      (let* ((input code-string)
             (indent-and-input
              (cond ((regexp-match #rx"\n( *)// *SLIDE START[^\n]*\n(.*)" input) =>
                     (lambda (elems) (list (cadr elems) (caddr elems))))
                    (else (list "" input))))
             (indent (car indent-and-input))
             (input (cadr indent-and-input))
             (input (cond ((regexp-match #rx"(.*)// *SLIDE FINIS" input) => cadr)
                          (else input)))
             (input (regexp-replace* #rx"\n([^\n]*// *SLIDE HIDE)" input ""))
             (input (regexp-replace* (string-append "\n" indent) input "\n"))
             (input (regexp-replace* (string-append "^" indent) input ""))
             )
        input)))

(define call-with-url-and-code
  (let ((handler
         (lambda (code-string f p-url)
           (let ((code (extract-code-to-present code-string)))
             ;; (display "Handling code ") (print code) (newline)
             (f p-url (rust-tt/nl code))))))
    (case-lambda
     ((code-string f)
      (handler code-string f (playpen-url #f (encode-playpen-url code-string))))
     ((tiny-url code-string f)
      (handler code-string f (playpen-url tiny-url tiny-url))))))

#;
(define (slide-code/tiny-url tiny-url title code-string)
  (call-with-url-and-code tiny-url code-string
                          (lambda (pu code)
                            (slide #:title title #:layout 'top pu code))))

#;
(define (slide-code/url title code-string)
  (call-with-url-and-code code-string
                          (lambda (pu code)
                            (slide #:title title #:layout 'top pu code))))
#;
(define (slide-code title code-string)
  (call-with-url-and-code code-string
                          (lambda (pu code)
                            (slide #:title title #:layout 'top code))))
