#lang racket

(require 2htdp/image)
(require (only-in lang/htdp-beginner make-posn))

(define STACK-WIDTH 120)
(define STACK-HEIGHT 300)
(define OBJECT-WIDTH 110)
(define LABEL-SIZE 24)
(define LABEL-HEIGHT 30)

(define CLEAR (color 255 255 255 0))
(define THICK-RED (make-pen "red" 8 "solid" "round" "round"))
(define THICK-BLACK (make-pen "black" 8 "solid" "round" "round"))
(define TRANSLUCENT-THICK-RED (make-pen (color 255 0 0 127) 8 "solid" "round" "round"))

(define SOLID-BLACK (make-pen "black" 3 "solid" "round" "round"))
(define DASHED-BLACK (make-pen "black" 3 "short-dash" "round" "round"))
(define EYE-BLACK (make-pen "black" 1 "solid" "round" "round"))

(define (shift-pinhole dx dy image)
  (put-pinhole (+ dx (pinhole-x image))
               (+ dy (pinhole-y image))
               image))

(define (top-pinhole image)
  (let ((px (or (pinhole-x image) (/ (image-width image) 2))))
    (put-pinhole px 0 image)))
(define (bot-pinhole image)
  (let ((px (or (pinhole-x image) (/ (image-width image) 2))))
    (put-pinhole px (image-height image) image)))

(define (rotate/pinhole angle image)
  (let* ((px (pinhole-x image))
         (py (pinhole-y image))
         (iw (image-width image))
         (ih (image-height image))
         (big-circ (circle
                    (max (- iw px)
                         (- ih py))
                    "solid"
                    CLEAR)))
    (rotate angle
            (overlay/pinhole
             image
             big-circ))))

;; bordered: Color Color (Mode Color -> Image)
(define (bordered inner-color border-color f . args)
  (overlay (apply f (append args (list "outline" border-color)))
           (apply f (append args (list "solid" inner-color)))))

(define STACK-FILL-COLOR "aqua")
(define STACK (top-pinhole (bordered STACK-FILL-COLOR "black" rectangle STACK-WIDTH STACK-HEIGHT)))
; (bordered "green" "black" circle (/ OBJECT-WIDTH 2))

(define LABEL-B
  (text "B" LABEL-SIZE "black"))

(define B-ON-STACK
  (center-pinhole (overlay LABEL-B
                           (bordered "green" "black" rectangle OBJECT-WIDTH OBJECT-WIDTH))))
(define B-ON-HEAP
  (center-pinhole (overlay LABEL-B
                           (bordered "green" "black" circle (/ OBJECT-WIDTH 2)))))

(define (ref label)
  (center-pinhole (overlay (text label LABEL-SIZE "black")
                           (bordered "white" "black" rectangle OBJECT-WIDTH LABEL-HEIGHT))))

(define (add-arrow-curve image x1 y1 angle1 pull1 x2 y2 angle2 pull2 pen-or-color)
  (let* ((px (pinhole-x image))
         (py (pinhole-y image))
         (image (overlay/pinhole (rotate/pinhole (+ angle2 45 180)
                                                 (overlay/pinhole
                                                  (put-pinhole 0 0 (line 8  0 pen-or-color))
                                                  (put-pinhole 0 0 (line 0  8 pen-or-color))))
                                 (put-pinhole x2 y2 image)))
         (image (put-pinhole px py image)))
    ; (display image)
    (add-curve image
               x1 y1 angle1 pull1
               x2 y2 angle2 pull2
               pen-or-color)))

(define (vec-1-to-n before/after)
  (let* ((after (eq? before/after 'after))
         (item (lambda (txt)
                 (overlay (text txt (* 1/2 LABEL-SIZE) "black")
                          (bordered "green" "black" circle (* 1/2 LABEL-SIZE)))))
         (texts (list (item "B1")
                      (item "B2")
                      (item "B3")
                      (text "."   LABEL-SIZE "black")
                      (text "."   LABEL-SIZE "black")
                      (text "."   LABEL-SIZE "black")
                      (item "Bn")))
         (texts (if after (append texts (list (item "Bn+1")))
                    texts))
         (boxes (apply above texts))
         (w (* 1/2 OBJECT-WIDTH))
         (h-b4 (* (if after 12 7) LABEL-SIZE)))
    (overlay/pinhole (top-pinhole boxes)
                     (top-pinhole (bordered "white" "black" rectangle w h-b4)))))


(define (vec-reallocation before/after)
  (let* ((after (eq? before/after 'after))
         (b-ycenter (* -3/7 OBJECT-WIDTH))
         (a-ycenter (* -1/4 OBJECT-WIDTH))
         (w-ycenter (* -3/2 OBJECT-WIDTH))
         (w-ytarget (- 0 (+ OBJECT-WIDTH 200)))
         (right-border (- STACK-WIDTH (/ (- STACK-WIDTH OBJECT-WIDTH) 2)))
         (curves (list (list right-border (* -1 a-ycenter) 0 0.7
                             (if after (+ 160) (+ 260)) (* -7/7 b-ycenter) -90 0.3 SOLID-BLACK)
                       ))
         (curves (if (not after) curves
                     (append (build-list 5 (lambda (i) (let ((y (+ (* 23 i) 60)))
                                                         (list 230 y 180 0.1
                                                               190 y 180 0.1 (make-color 220 100 0)))))
                             curves)))
         (annotations (list (list "rw" (if after -170 -270) -20)
                            ))
         )
    (foldl
     (lambda (annotation base) (overlay/xy (text (list-ref annotation 0) LABEL-SIZE "black")
                                           (list-ref annotation 1)
                                           (list-ref annotation 2)
                                           base))
     (foldl (lambda (curve base) (apply add-arrow-curve base curve))
            (overlay/pinhole (shift-pinhole -200 -100
                                            (center-pinhole
                                             (rotate 45 (line 100 0
                                                              (if after THICK-BLACK CLEAR)))))
                             (shift-pinhole 0 (* 1 a-ycenter) (ref "a"))
                             (if after
                                 (shift-pinhole (- 100) b-ycenter (vec-1-to-n 'after))
                                 (rectangle 10 10 "solid" CLEAR))
                             (shift-pinhole (- 200) b-ycenter (vec-1-to-n 'before))
                             STACK)
            curves)
     annotations)))

(define vec-reallocation-before (vec-reallocation 'before))
(define vec-reallocation-after (vec-reallocation 'after))

(define pristine-vec
  (let* ((b-ycenter (* -3/7 OBJECT-WIDTH))
         (a-ycenter (* -1/4 OBJECT-WIDTH))
         (w-ycenter (* -3/2 OBJECT-WIDTH))
         (w-ytarget (- 0 (+ OBJECT-WIDTH 200)))
         (right-border (- STACK-WIDTH (/ (- STACK-WIDTH OBJECT-WIDTH) 2)))
         (curves (list (list right-border (* -1 a-ycenter) 0 0.7
                             (if #f (+ 160) (+ 260)) (* -7/7 b-ycenter) -90 0.3 SOLID-BLACK)
                       ))
         (annotations (list (list "rw" (if #f -170 -270) -20)))
         )
    (foldl
     (lambda (annotation base) (overlay/xy (text (list-ref annotation 0) LABEL-SIZE "black")
                                           (list-ref annotation 1)
                                           (list-ref annotation 2)
                                           base))
     (foldl (lambda (curve base) (apply add-arrow-curve base curve))
            (overlay/pinhole (shift-pinhole 0 (* 1 a-ycenter) (ref "a"))
                             (rectangle 10 10 "solid" CLEAR)
                             (shift-pinhole (- 200) b-ycenter (vec-1-to-n 'before))
                             STACK)
            curves)
     annotations)))

(define (v-range width height color-or-pen)
  (let ((h (center-pinhole (line width 0 color-or-pen)))
        (v (center-pinhole (line 0 height color-or-pen))))
    (clear-pinhole (overlay/pinhole (shift-pinhole 0 (* -1/2 height) h)
                                    v
                                    (shift-pinhole 0 (*  1/2 height) h)))))
                     

(define vec-with-two-disjoint-imm-slices
  (let* ((b-ycenter (* -3/7 OBJECT-WIDTH))
         (a-ycenter (* -1/4 OBJECT-WIDTH))
         (r1-ycenter (* -3/4 OBJECT-WIDTH))
         (r2-ycenter (* -5/4 OBJECT-WIDTH))
         (w-ycenter (* -3/2 OBJECT-WIDTH))
         (w-ytarget (- 0 (+ OBJECT-WIDTH 200)))
         (range-dim LABEL-SIZE)
         (top-range-y (* 7/2 range-dim))
         (bot-range-y (* 3/2 range-dim))
         (right-border (- STACK-WIDTH (/ (- STACK-WIDTH OBJECT-WIDTH) 2)))
         (curves (list (list right-border (* -1 a-ycenter) 0 0.7
                             (+ 260) (* -7/7 b-ycenter) -90 0.3 SOLID-BLACK)
                       (list right-border (* -1 r1-ycenter) 0 0.7
                             (+ 200) (* 1 top-range-y) 0 0.3 DASHED-BLACK)
                       (list right-border (* -1 r2-ycenter) 0 0.7
                             (+ 200) (* 9/2 bot-range-y) 0 0.3 DASHED-BLACK)
                       
                       ))
         (annotations (list (list "r" -270 -20)
                            (list "r" -190 -60)
                            (list "r" -190 -130)
                            ))
         )
    (foldl
     (lambda (annotation base) (overlay/xy (text (list-ref annotation 0) LABEL-SIZE "black")
                                           (list-ref annotation 1)
                                           (list-ref annotation 2)
                                           base))
     (foldl (lambda (curve base) (apply add-arrow-curve base curve))
            (overlay/pinhole (shift-pinhole 0 (* 1 r1-ycenter) (ref "r1"))
                             (shift-pinhole 0 (* 1 r2-ycenter) (ref "r2"))
                             (shift-pinhole 0 (* 1 a-ycenter) (ref "a"))
                             (shift-pinhole (- 150) b-ycenter
                                            (top-pinhole
                                             (v-range (* 1/2 range-dim) top-range-y "black")))
                             (shift-pinhole (- 150) (* 3 b-ycenter)
                                            (top-pinhole
                                             (v-range (* 1/2 range-dim) bot-range-y "black")))
                             (shift-pinhole (- 200) b-ycenter (vec-1-to-n 'before))
                             STACK)
            curves)
     annotations)))

(define vec-with-two-overlapping-imm-slices
  (let* ((b-ycenter (* -3/7 OBJECT-WIDTH))
         (a-ycenter (* -1/4 OBJECT-WIDTH))
         (r1-ycenter (* -3/4 OBJECT-WIDTH))
         (r2-ycenter (* -5/4 OBJECT-WIDTH))
         (w-ycenter (* -3/2 OBJECT-WIDTH))
         (w-ytarget (- 0 (+ OBJECT-WIDTH 200)))
         (range-dim LABEL-SIZE)
         (top-range-y (* 8/2 range-dim))
         (bot-range-y (* 8/2 range-dim))
         (right-border (- STACK-WIDTH (/ (- STACK-WIDTH OBJECT-WIDTH) 2)))
         (curves (list (list right-border (* -1 a-ycenter) 0 0.7
                             (+ 260) (* -7/7 b-ycenter) -90 0.3 SOLID-BLACK)
                       (list right-border (* -1 r1-ycenter) 0 0.7
                             (+ 200) (* 4/5 top-range-y) 0 0.3 DASHED-BLACK)
                       (list right-border (* -1 r2-ycenter) 0 0.7
                             (+ 200) (* 3/2 bot-range-y) 0 0.3 DASHED-BLACK)
                       
                       ))
         (annotations (list (list "r" -270 -20)
                            (list "r" -190 -50)
                            (list "r" -190 -120)
                            ))
         )
    (foldl
     (lambda (annotation base) (overlay/xy (text (list-ref annotation 0) LABEL-SIZE "black")
                                           (list-ref annotation 1)
                                           (list-ref annotation 2)
                                           base))
     (foldl (lambda (curve base) (apply add-arrow-curve base curve))
            (overlay/pinhole (shift-pinhole 0 (* 1 r1-ycenter) (ref "r1"))
                             (shift-pinhole 0 (* 1 r2-ycenter) (ref "r2"))
                             (shift-pinhole 0 (* 1 a-ycenter) (ref "a"))
                             (shift-pinhole (- 160) b-ycenter
                                            (top-pinhole
                                             (v-range (* 1/2 range-dim) top-range-y "black")))
                             (shift-pinhole (- 145) (* 2 b-ycenter)
                                            (top-pinhole
                                             (v-range (* 1/2 range-dim) bot-range-y "black")))
                             (shift-pinhole (- 200) b-ycenter (vec-1-to-n 'before))
                             STACK)
            curves)
     annotations)))

(define vec-with-whole-mut-slice
  (let* ((b-ycenter (* -3/7 OBJECT-WIDTH))
         (a-ycenter (* -1/4 OBJECT-WIDTH))
         (w-ycenter (* -3/4 OBJECT-WIDTH))
         (w-ytarget (- 0 (+ OBJECT-WIDTH 200)))
         (range-dim LABEL-SIZE)
         (top-range-y (* 14/2 range-dim))
         (bot-range-y (* 3/2 range-dim))
         (right-border (- STACK-WIDTH (/ (- STACK-WIDTH OBJECT-WIDTH) 2)))
         (curves (list (list right-border (* -1 a-ycenter) 0 0.7
                             (+ 260) (* -7/7 b-ycenter) -90 0.3 SOLID-BLACK)
                       (list right-border (* -1 w-ycenter) 0 0.7
                             (+ 200) (* 1/2 top-range-y) 0 0.3 DASHED-BLACK)
                       ))
         (annotations (list 
                            (list "rw" -180 -60)
                            ))
         )
    (foldl
     (lambda (annotation base) (overlay/xy (text (list-ref annotation 0) LABEL-SIZE "black")
                                           (list-ref annotation 1)
                                           (list-ref annotation 2)
                                           base))
     (foldl (lambda (curve base) (apply add-arrow-curve base curve))
            (overlay/pinhole (shift-pinhole 0 (* 1 w-ycenter) (ref "w"))
                             (shift-pinhole 0 (* 1 a-ycenter) (ref "a"))
                             (shift-pinhole (- 150) b-ycenter
                                            (top-pinhole
                                             (v-range (* 1/2 range-dim) top-range-y "black")))
                             (shift-pinhole (- 200) b-ycenter (vec-1-to-n 'before))
                             STACK)
            curves)
     annotations)))

(define vec-with-two-disjoint-mut-slices
  (let* ((b-ycenter (* -3/7 OBJECT-WIDTH))
         (a-ycenter (* -1/4 OBJECT-WIDTH))
         (w1-ycenter (* -3/4 OBJECT-WIDTH))
         (w2-ycenter (* -5/4 OBJECT-WIDTH))
         (w-ycenter (* -3/2 OBJECT-WIDTH))
         (w-ytarget (- 0 (+ OBJECT-WIDTH 200)))
         (range-dim LABEL-SIZE)
         (top-range-y (* 7/2 range-dim))
         (bot-range-y (* 6/2 range-dim))
         (right-border (- STACK-WIDTH (/ (- STACK-WIDTH OBJECT-WIDTH) 2)))
         (curves (list (list right-border (* -1 a-ycenter) 0 0.7
                             (+ 260) (* -7/7 b-ycenter) -90 0.3 SOLID-BLACK)
                       (list right-border (* -1 w1-ycenter) 0 0.7
                             (+ 200) (* 1 top-range-y) 0 0.3 DASHED-BLACK)
                       (list right-border (* -1 w2-ycenter) 0 0.7
                             (+ 200) (* 5/2 bot-range-y) 0 0.3 DASHED-BLACK)
                       
                       ))
         (annotations (list (list "rw" -180 -60)
                            (list "rw" -180 -140)
                            ))
         )
    (foldl
     (lambda (annotation base) (overlay/xy (text (list-ref annotation 0) LABEL-SIZE "black")
                                           (list-ref annotation 1)
                                           (list-ref annotation 2)
                                           base))
     (foldl (lambda (curve base) (apply add-arrow-curve base curve))
            (overlay/pinhole (shift-pinhole 0 (* 1 w1-ycenter) (ref "w1"))
                             (shift-pinhole 0 (* 1 w2-ycenter) (ref "w2"))
                             (shift-pinhole 0 (* 1 a-ycenter) (ref "a"))
                             (shift-pinhole (- 150) b-ycenter
                                            (top-pinhole
                                             (v-range (* 1/2 range-dim) top-range-y "black")))
                             (shift-pinhole (- 150) (* 3 b-ycenter)
                                            (top-pinhole
                                             (v-range (* 1/2 range-dim) bot-range-y "black")))
                             (shift-pinhole (- 200) b-ycenter (vec-1-to-n 'before))
                             STACK)
            curves)
     annotations)))

(define-syntax save-svg
  (syntax-rules ()
    ((save-svg NAME)
     (save-svg-image (scale 3/4 (clear-pinhole NAME)) (string-append "/tmp/" (symbol->string 'NAME) ".svg")))))

(define-syntax save-svgs
  (syntax-rules ()
    ((save-svgs NAME ...)
     (begin (save-svg NAME) ... (list NAME ...)))))

(save-svgs vec-reallocation-before
           vec-reallocation-after
           pristine-vec
           vec-with-two-disjoint-imm-slices
           vec-with-two-overlapping-imm-slices
           vec-with-whole-mut-slice
           vec-with-two-disjoint-mut-slices
           )
