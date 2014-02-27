#lang racket
(require slideshow)
(require slideshow/balloon)
(require "rust-meetup2014-common.rkt")

(slide #:name "Concurrency"
       (t "Concurrency"))


;; AHHH XXX FIXME

(define (stack hdot-1 hdot-2 odot o-idx)
  (let* ((r (lambda () (rectangle 80 10)))
         (r1 (r))
         (r2 (r)))
    (apply vl-append (build-list 12
                                 (lambda (i)
                                   (cond ((= i 2)
                                          (cc-superimpose (r) (hc-append 20 hdot-1 (blank 3))))
                                         ((= i 5)
                                          (cc-superimpose (r) (hc-append 20 (blank 3) hdot-2)))
                                         ((= i o-idx)
                                          (cc-superimpose (r) odot))
                                         (else
                                          (r))))))))

(define (task-state odot o-idx)
  (let* ((hdot-1 (colorize (disk 3) "blue"))
         (hdot-2 (colorize (disk 3) "blue"))
         (stk (stack hdot-1 hdot-2 odot o-idx))
         (heap (rounded-rectangle 160 160))
         (obj1 (circle 30))
         (heap (pin-over heap 50 40 obj1))
         (task (vc-append 20 heap stk))
         (task (pin-arrow-line 10 task
                              hdot-1 cc-find obj1 cb-find))
         (task (pin-arrow-line 10 task
                               hdot-2 cc-find obj1 cb-find)))
    task))

(slide (task-state (blank 3) 7))

(let ((odot (blank 3)))
  (slide (hc-append gap-size
                    (task-state odot 7)
                    (task-state odot 11))))

(let* ((odot1 (colorize (disk 3) "maroon"))
       (odot2 (colorize (disk 3) "maroon"))
       (t1 (task-state odot1 7))
       (t2 (task-state odot2 11))
       (xchg-heap (rounded-rectangle 400 160))
       (obj1 (colorize (cc-superimpose (circle 30) (text "T" null 16)) "maroon"))
       (xchg-heap (pin-over xchg-heap 50 40 obj1))
       (global (vc-append gap-size
                          (hc-append gap-size t1 t2)
                          xchg-heap)))
  (slide
   (rust-tt "let o = ~make_t(); ...")
   'alts
   (list (list (pin-arrow-line 10 global odot1 cc-find obj1 ct-find))
         (list (pin-balloon
                (wrap-balloon (t "~T") 'se -5 3)
                (pin-arrow-line 10 global odot1 cc-find obj1 ct-find)
                odot1
                lc-find))
         )))

(let* ((odot1 (colorize (disk 3) "maroon"))
       (odot2 (colorize (disk 4) "maroon"))
       (t1 (task-state odot1 7))
       (t2 (task-state odot2 11))
       (xchg-heap (rounded-rectangle 400 160))
       (obj1 (colorize (cc-superimpose (circle 30) (text "T" null 16)) "maroon"))
       (xchg-heap (pin-over xchg-heap 50 40 obj1))
       (global (vc-append gap-size
                          (hc-append gap-size t1 t2)
                          xchg-heap)))
  (slide
   (rust-tt "... chan.send(o); /* o is now locally invalid */")
   (pin-arrow-line 10 global odot2 cc-find obj1 ct-find)))

(slide (t "(totally different: circles demo)"))
