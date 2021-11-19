;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part C - Racket tutorial  ; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Step 2 is done for you:
#lang slideshow

; Step 4: "Definitions"
(circle 10)

(hc-append 20 (circle 10) (circle 20))

(vc-append (circle 10) (circle 20))

(define c (circle 10))
(define r (rectangle 10 20))
(vc-append c r c) ;vertically appending cols

(define (square n)
 ;create a n x n square
  (filled-rectangle n n))
(square 10)

; Step 5: "Local Binding"

(define (four p)
;create a suqure like shape consists of four objects
  (define two-p (hc-append p p))
  (vc-append two-p two-p))
(four (circle 10))

(define (checker p1 p2)
#|create a squre consists of four objects where the colors
are not consistent
|#
  (let ([p12 (hc-append p1 p2 )]
        [p21 (hc-append p2 p1)])
    (vc-append p12 p21)))

(define (checkerboard p)
 #|creates a checkerboard of red and black
|#
  (let* ([rp (colorize p "red")]
        [bp (colorize p "black")]
        [c (checker rp bp)]
        [c4 (four c)])
  (four c4)))

; Step 6: "Functions are Values"
(define (series mk)
;horizontally appending mk of different sizes
  (hc-append 4 (mk 5) (mk 10) (mk 20)))

(series (lambda (size) (checkerboard (square size))))
#|mk is a function that takes in one input (named size) and
calls square with size, and then the result of the call to
square is passed as an argument to checkerboard. 
|#

(define (rgb-series mk)
  (vc-append
   (series (lambda (sz) (colorize (mk sz) "red")))
   (series (lambda (sz) (colorize (mk sz) "green")))
   (series (lambda (sz) (colorize (mk sz) "blue")))))

(define (rgb-maker mk)
  (lambda (sz)
    (vc-append (colorize (mk sz) "red")
               (colorize (mk sz) "green")
               (colorize (mk sz) "blue"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part D - Reflect          ; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; You'll type this directly in Gradescope
#|When creating a function or varible, racket always uses define() which
actually made things easier. What confuses me a lot is the use of lambda,
the idea of anonymous function, hopefully I'll figure out how it works. It
was also to my surprise that Racket can show colored images(shapes) directly console, unlike
python, which requires another window.
 |#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part E - cboard function  ; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cboard n color1 color2)
 ;create a checkboard with n indicating the size
  (let*([rp (colorize (square n) color1)]
        [bp (colorize (square n) color2)]
        [c (checker rp bp)]
        [c4 (four c)])
    (four c4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part F - hcomb function   ; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (three p)
;create a square like shape
  (define two-p (hc-append p p))
  (vl-append two-p two-p))
(three (circle 10))

(define (comb p1 p2 p3)
#|combine three different colored shapes into
a 9x9 square
|#
  (let ([p123 (hc-append p1 p2 p3 )]
        [p231 (hc-append p2 p3 p1)]
        [p312 (hc-append p3 p1 p2)])
    (vl-append p123 p231 p312)))

(define (hcomb n color1 color2 color3)
 #|set colors to the circles and combine four 9x9
squres together
|#
  (let* ([fst (colorize (filled-ellipse n n) color1)]
        [snd (colorize (filled-ellipse n n) color2)]
        [third (colorize (filled-ellipse n n) color3)]
        [h (comb fst snd third)]
        [h3 (three h)])
    (three h3)))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part G - Screen shots!    ; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Don't forget to take screen shots of your work!


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part H - Self-teaching    ; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
