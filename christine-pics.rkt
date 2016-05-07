#lang racket

(require 2htdp/image)
(require (only-in lang/htdp-beginner make-posn))
(define house
  (above (triangle 40 "solid" "red")
         (rectangle 40 30 "solid" "black")))

(define h+c
  (add-curve house
             0   0   0   1/3
             100 100 180 2/3 "blue"))

(define t (text "Hello World" 24 "green"))

(define combined
  (above h+c
         t))

(save-svg-image house "/tmp/house.svg")
(save-svg-image combined "/tmp/combined.svg")
combined

(foldl + 0 '(1 2 3))

(define (shift-pinhole dx dy image)
  (put-pinhole (+ dx (pinhole-x image))
               (+ dy (pinhole-y image))
               image))

(define (top-pinhole image)
  (put-pinhole (pinhole-x image) 0 image))
(define (bot-pinhole image)
  (put-pinhole (pinhole-x image) (image-height image) image))

(define CLEAR (color 255 255 255 0))
(define THICK-RED (make-pen "red" 8 "solid" "round" "round"))
(define EYE-BLACK (make-pen "black" 1 "solid" "round" "round"))

(define BODY-OFFSET -20)

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

(define (car hood-angle)
  (let* ((wheel (center-pinhole (circle 20 "solid" "black")))
         (hood (put-pinhole 70 1 (rectangle  70 2 "solid" "red")))
         (bump (rectangle 3 4 "solid" "black"))
         (space (rectangle 3 1 "solid" CLEAR)) 
         (bumper (center-pinhole (above bump space bump space bump)))
         (door (center-pinhole (rectangle 50 25 "outline" "black")))
         (door-handle (center-pinhole (rectangle 8 2 "solid" "black")))
         (front-wheel-x-offset -60)
         (top (center-pinhole (add-curve
                               (rectangle 200 30 0 CLEAR)
                               65   1   30 1/3
                               150   1  -80 2/3 THICK-RED))))
    ;(display top)
    (overlay/pinhole (shift-pinhole  -10    2 door)
                     (shift-pinhole    0   10 top)
                     (shift-pinhole   30   10 (rotate/pinhole hood-angle hood))
                     (shift-pinhole  101   -5 bumper)
                     (shift-pinhole  -25    5 door-handle)
                     (rectangle 200 30 "solid" "red")
                     (shift-pinhole front-wheel-x-offset     -10 wheel)
                     (shift-pinhole (- front-wheel-x-offset) -10 wheel)
                     )))

(define (driver-pinhole car)
  (shift-pinhole 15 -23 car))

(define (inspector-pinhole car)
  (shift-pinhole -65 -25 car))

(define (corpse-pinhole car)
  (shift-pinhole -60 50 car))

(define BODY-SIZE 15)

(define BODY-CIRCLE
  (center-pinhole
   (overlay (circle BODY-SIZE "outline" "black")
            (circle BODY-SIZE "solid" "green"))))

(define inspector-body
  (let ((legs-width 20)
        (legs-height 30))
    (shift-pinhole 0 BODY-OFFSET
                   (overlay/pinhole
                    (center-pinhole BODY-CIRCLE)
                    (top-pinhole (center-pinhole (overlay (rectangle legs-width legs-height "outline" "black")
                                                          (rectangle legs-width legs-height "solid" "green"))))
                    ))))

(define EYE (center-pinhole (rectangle 3 2 "solid" "blue")))

(define (corpse left-arm-twist right-arm-twist)
  (let* ((strand (shift-pinhole 0 -8 (center-pinhole (isosceles-triangle 5 -60 "solid" "black"))))
         (eye-line (line 4 0 EYE-BLACK))
         (cross-eye (center-pinhole (overlay (rotate 45 eye-line) (rotate -45 eye-line))))
         (arm-length 20)
         (arm-width 8)
         (arm (shift-pinhole (* -1/2 arm-length) 0
                             (center-pinhole (overlay/pinhole (rectangle arm-length arm-width "solid" "green")
                                                              (rectangle arm-length arm-width "outline" "black")
                                                              (shift-pinhole -12 0 (center-pinhole (rectangle (/ arm-length 4) arm-width "outline" "black")))
                                                              (shift-pinhole -12 0 (center-pinhole (rectangle (/ arm-length 4) arm-width "solid" "green")))
                                                              ))))
         )
    (overlay/pinhole (shift-pinhole -4 0 cross-eye)
                     (shift-pinhole  4 0 cross-eye)
                     (circle 8 "outline" "black")
                     (circle 8 "solid" "green")
                     (rotate  75 strand)
                     (rotate  60 strand)
                     (rotate  45 strand)
                     (rotate  30 strand)
                     (rotate  15 strand)
                     (rotate   0 strand)
                     (rotate -15 strand)
                     (rotate -30 strand)
                     (rotate -45 strand)
                     (rotate -60 strand)
                     (rotate -75 strand)
                     (apply shift-pinhole (append left-arm-twist (list (rotate 30 arm))))
                     (apply shift-pinhole (append right-arm-twist (list (rotate 150 arm))))
                     (shift-pinhole 0 0 (center-pinhole (overlay (ellipse (* 7/4 BODY-SIZE) BODY-SIZE "solid" "green")
                                                                 (ellipse (* 7/4 BODY-SIZE) BODY-SIZE "outline" "black")
                                                                 )))
                     )))

(define inspector-front
  (let ((strand (center-pinhole (isosceles-triangle 5 -60 "solid" "black")))
        (eye EYE))
    (overlay/pinhole (shift-pinhole -3 0 eye)
                     (shift-pinhole 3 0 eye)
                     (circle 8 "outline" "black")
                     (circle 8 "solid" "green")
                     (shift-pinhole 7 0 strand)
                     (shift-pinhole -7 0 strand)
                     inspector-body
                     )))
(define inspector-back
  (let ((strand (center-pinhole (isosceles-triangle 5 -60 "solid" "black"))))
    (overlay/pinhole inspector-body
                     (shift-pinhole  -7 0 strand)
                     (shift-pinhole  -5 0 strand)
                     (shift-pinhole  -3 0 strand)
                     (shift-pinhole  -1 0 strand)
                     (shift-pinhole   1 0 strand)
                     (shift-pinhole   3 0 strand)
                     (shift-pinhole   5 0 strand)
                     (shift-pinhole   7 0 strand)
                     (circle 8 "outline" "black")
                     (circle 8 "solid" "green")
                     )))
  
(define (driver mouth-angle)
  (let ((eye EYE)
        (strand (shift-pinhole 0 10 (center-pinhole (isosceles-triangle 5 60 "solid" "black")))))
    (overlay/pinhole
     (rotate/pinhole 45 strand)
     (rotate/pinhole 30 strand)
     (rotate/pinhole 15 strand)
     (rotate/pinhole 0 strand)
     (rotate/pinhole -15 strand)
     (rotate/pinhole -30 strand)
     (rotate/pinhole -45 strand)
     (rotate/pinhole -60 strand)
     (rotate/pinhole -75 strand)
     (shift-pinhole 5 0 eye)
     (shift-pinhole 6 -5 (rotate/pinhole mouth-angle (shift-pinhole -2 0 (center-pinhole (rectangle 4 1 "solid" "black")))))
     (center-pinhole (circle 8 "outline" "black"))
     (center-pinhole (circle 8 "solid" "aqua"))
     (shift-pinhole -2 BODY-OFFSET
                    (overlay/pinhole (center-pinhole (circle BODY-SIZE "outline" "black"))
                                     (center-pinhole (circle BODY-SIZE "solid" "aqua")))))))

(define SCALE 2)

(define show-one-inspector
  (scale SCALE (overlay/pinhole (inspector-pinhole (car -30)) inspector-front)))

(define show-many-inspectors
  (scale SCALE (overlay/pinhole (shift-pinhole  15 -5 inspector-back)
                                (shift-pinhole -20 -5 inspector-back)
                                (inspector-pinhole (car -30))
                                (shift-pinhole  20  0 inspector-front)
                                inspector-front)))

(define (speed-lines adj)
  (let ((line (center-pinhole (rectangle 15 1 "solid" "black")))
        (space (center-pinhole (rectangle 3 10 "solid" CLEAR))))
    (shift-pinhole -100 -30 (center-pinhole (above/align "pinhole"
                                                         (shift-pinhole (* adj 0) 0 line)
                                                         space
                                                         (shift-pinhole (* adj 1) 0 line)
                                                         space
                                                         (shift-pinhole (* adj 2) 0 line))))))

(define show-driver-alone
  (scale SCALE (overlay/pinhole (driver-pinhole (car 0))
                                (driver 15)
                                (speed-lines 0))))

(define show-driver-and-corpses
  (scale SCALE (overlay/pinhole (shift-pinhole 60 110 (speed-lines 2))
                                (shift-pinhole -10 6 (rotate/pinhole 180 (corpse '(5 -2) '(-3 -5))))
                                (corpse-pinhole (overlay/pinhole (driver-pinhole (car -100)) (driver -15) (speed-lines 0)))
                                (shift-pinhole 35 4 (corpse '(8 0) '(6 -3)))
                                )))

(define pristine-car (scale SCALE (car 0)))
(define open-hood-car (scale SCALE (car -30)))

pristine-car
show-one-inspector
show-many-inspectors
show-driver-alone
show-driver-and-corpses

(define-syntax save-svg
  (syntax-rules ()
    ((save-svg NAME)
     (save-svg-image (clear-pinhole NAME) (string-append "/tmp/" (symbol->string 'NAME) ".svg")))))

(save-svg pristine-car)
(save-svg show-one-inspector)
(save-svg show-many-inspectors)
(save-svg show-driver-alone)
(save-svg show-driver-and-corpses)