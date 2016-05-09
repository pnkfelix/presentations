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

(define stack-allocation
  (overlay/pinhole (shift-pinhole 0 (* -4/7 OBJECT-WIDTH) B-ON-STACK)
                   STACK))

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

(define stack-allocation-with-r1/r2
  (let* ((b-ycenter (* -4/7 OBJECT-WIDTH))
         (r1-ycenter (* -12/7 OBJECT-WIDTH))
         (r2-ycenter (* -15/7 OBJECT-WIDTH))
         (right-border (- STACK-WIDTH (/ (- STACK-WIDTH OBJECT-WIDTH) 2)))
         (curves (list (list right-border (- r1-ycenter)  30 0.5
                             right-border (* 5/4 (- b-ycenter))   160 0.5 DASHED-BLACK)
                       (list right-border (- r2-ycenter)  30 0.7
                             right-border (* 3/4 (- b-ycenter))   160 0.6 DASHED-BLACK)))
         )
    (foldl (lambda (curve base) (apply add-arrow-curve base curve))
           (overlay/pinhole (shift-pinhole 0 b-ycenter B-ON-STACK)                   
                            (shift-pinhole 0 r1-ycenter (ref "r1"))
                            (shift-pinhole 0 r2-ycenter (ref "r2"))
                            STACK)
           curves)))

(define stack-allocation-with-w
  (let* ((b-ycenter (* -4/7 OBJECT-WIDTH))
         (w-ycenter (* -15/7 OBJECT-WIDTH))
         (right-border (- STACK-WIDTH (/ (- STACK-WIDTH OBJECT-WIDTH) 2)))
         (curves (list (list right-border (- w-ycenter)  30 0.7
                             right-border (* 3/4 (- b-ycenter))   160 0.6 DASHED-BLACK)))
         )
    (foldl (lambda (curve base) (apply add-arrow-curve base curve))
           (overlay/pinhole (shift-pinhole 0 b-ycenter B-ON-STACK)                   
                            (shift-pinhole 0 w-ycenter (ref "w"))
                            STACK)
           curves)))

(define (image-length img)
  (min (image-height img) (image-width img)))

(define (surround img color)
  (overlay (circle (* 22/20 (image-length img)) "outline" (make-pen color 8 "solid" "round" "round"))
           img))

(define (allow img)
  (surround img (make-color 0 255 0 127)))

(define (disallow img)
  (let ((length (image-length img)))
    (overlay
     (rotate -45 (line 0 (* 2 length) TRANSLUCENT-THICK-RED))
     (surround img (make-color 255 0 0 127)))))



(define stack-b-with-r1-and-r2
  (foldr (lambda (text base) (apply overlay/xy (append text (list base))))
         stack-allocation-with-r1/r2
         (list (list (text "r" LABEL-SIZE "black") (- 0 (+ STACK-WIDTH 40)) -30 )
               (list (text "r" LABEL-SIZE "black") (- 0 (+ STACK-WIDTH 40)) -60 )
               (list (disallow (text "w" LABEL-SIZE "black")) 30 -10 )
               )))

(define stack-b-with-w
  (foldr (lambda (text base) (apply overlay/xy (append text (list base))))
         stack-allocation-with-w
         (list (list (text "rw" LABEL-SIZE "black") (- 0 (+ STACK-WIDTH 80)) -20 )
               (list (disallow (text "r " LABEL-SIZE "black")) 40 -10 )
               (list (disallow (text "w" LABEL-SIZE "black")) 30 -10 )
               )))

(define stack-a-owns-heap-b
  (let* ((b-ycenter (* -4/7 OBJECT-WIDTH))
         (a-ycenter (* -1/2 OBJECT-WIDTH))
         (right-border (- STACK-WIDTH (/ (- STACK-WIDTH OBJECT-WIDTH) 2)))
         (curves (list (list right-border (* -1 a-ycenter) 0 0.7
                             (+ 205) (* -1 b-ycenter) 0 0.7 SOLID-BLACK)) ))
    (overlay/xy
     (text "rw" LABEL-SIZE "black")
     -180 -35
     (foldl (lambda (curve base) (apply add-arrow-curve base curve))
            (overlay/pinhole (shift-pinhole 0 (* 1 a-ycenter) (ref "a"))
                             (shift-pinhole (- 200) b-ycenter B-ON-HEAP)
                             STACK)
            curves))))


(define stack-a-owns-heap-b-borrows-r1-and-r2
  (let* ((b-ycenter (* -4/7 OBJECT-WIDTH))
         (a-ycenter (* -1/2 OBJECT-WIDTH))
         (ra-ycenter (* -3/2 OBJECT-WIDTH))
         (r1-ycenter (* -4/2 OBJECT-WIDTH))
         (r2-ycenter (* -5/2 OBJECT-WIDTH))
         (ra-ytarget a-ycenter)
         (r1-ytarget (* -11/12 OBJECT-WIDTH))
         (r2-ytarget (* -13/12 OBJECT-WIDTH))
         (left-border (/ (- STACK-WIDTH OBJECT-WIDTH) 2))
         (right-border (- STACK-WIDTH (/ (- STACK-WIDTH OBJECT-WIDTH) 2)))
         (curves (list (list right-border (* -1 a-ycenter) 0 0.7
                             (+ 205) (* -1 a-ycenter) 0 0.7 SOLID-BLACK)
                       (list right-border (* -1 ra-ycenter) 0 0.7
                             (- right-border 20) (+ (* -1 ra-ytarget) 20) 100 0.2 DASHED-BLACK)
                       (list right-border (* -1 r1-ycenter) 0 0.3
                             (+ 220) (* -1 r1-ytarget) 45 0.2 DASHED-BLACK)
                       (list right-border (* -1 r2-ycenter) 0 0.3
                             (+ 250) (* -1 r2-ytarget) 90 0.5 DASHED-BLACK)
                       ))
         (annotations (list (list "r" -195 -25)
                            (list "r" -200 -85)
                            (list "r" -235 -115))
                      ))
    (foldl
     (lambda (annotation base) (overlay/xy (text (list-ref annotation 0) LABEL-SIZE "black")
                                           (list-ref annotation 1)
                                           (list-ref annotation 2)
                                           base))
     (foldl (lambda (curve base) (apply add-arrow-curve base curve))
            (overlay/pinhole (shift-pinhole 0 (* 1 a-ycenter) (ref "a"))
                             (shift-pinhole 0 (* 1 ra-ycenter) (ref "r_of_box"))
                             (shift-pinhole 0 (* 1 r1-ycenter) (ref "r1: &B"))
                             (shift-pinhole 0 (* 1 r2-ycenter) (ref "r2: &B"))
                             (shift-pinhole (- 200) b-ycenter B-ON-HEAP)
                             STACK)
            curves)
     annotations)))

(define stack-a-owns-heap-b-mut-borrow-w
  (let* ((b-ycenter (* -4/7 OBJECT-WIDTH))
         (a-ycenter (* -1/2 OBJECT-WIDTH))
         (w-ycenter (* -3/2 OBJECT-WIDTH))
         (w-ytarget (* -11/12 OBJECT-WIDTH))
         (right-border (- STACK-WIDTH (/ (- STACK-WIDTH OBJECT-WIDTH) 2)))
         (curves (list (list right-border (* -1 a-ycenter) 0 0.7
                             (+ 205) (* -1 a-ycenter) 0 0.7 SOLID-BLACK)
                       (list right-border (* -1 w-ycenter) 0 0.3
                             (+ 220) (* -1 w-ytarget) 45 0.2 DASHED-BLACK)
                       ))
         (annotations (list (list "rw" -210 -110)
                            ))
         )
    (foldl
     (lambda (annotation base) (overlay/xy (text (list-ref annotation 0) LABEL-SIZE "black")
                                           (list-ref annotation 1)
                                           (list-ref annotation 2)
                                           base))
     (foldl (lambda (curve base) (apply add-arrow-curve base curve))
            (overlay/pinhole (shift-pinhole 0 (* 1 a-ycenter) (ref "a"))
                             (shift-pinhole 0 (* 1 w-ycenter) (ref "w: &mut B"))
                             (shift-pinhole (- 200) b-ycenter B-ON-HEAP)
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

(list 
 stack-allocation
 stack-b-with-r1-and-r2
 stack-b-with-w
 stack-a-owns-heap-b
 stack-a-owns-heap-b-borrows-r1-and-r2
 stack-a-owns-heap-b-mut-borrow-w
 )

stack-a-owns-heap-b-borrows-r1-and-r2

(save-svg stack-allocation)
(save-svg stack-b-with-r1-and-r2)
(save-svg stack-b-with-w)
(save-svg stack-a-owns-heap-b)
(save-svg stack-a-owns-heap-b-borrows-r1-and-r2)
(save-svg stack-a-owns-heap-b-mut-borrow-w)
