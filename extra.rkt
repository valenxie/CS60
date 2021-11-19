#lang slideshow
(define (eyes p)
;create a square like shape
  ;(define two-p (hc-append p p))
  (hc-append p p))
  
(define (comb p1 p2 p4 p5)
#|combine three different colored shapes into
a 9x9 square
|#
  (let ([p1 (hc-append p4 p1 p5 p1 )]
        [p2 (hc-append p5 p1 p4 p5)]
        [p3 (hc-append p1 p2 p2 p4)]
        [p4 (hc-append p4 p2 p2 p5)])
    (vl-append p1 p2 p3 p4)))

(define (ncomb p1 p2 p4 p5)
  (let ([p1 (hc-append p1 p4 p5 p2)]
        [p2 (hc-append p2 p5 p1 p4)])
    (hc-append p1 p2)))

(define (mouth p)
  ;(define two-p (hc-append p p))
  (vc-append p p))

(define (mcomb p1 p2 p4)
  (let ([p1 (hc-append p4 p1 p2 p2)]
        [p2 (hc-append p2 p2 p4 p1)])
    (hc-append p1 p2)))

(define (jawcomb p1 p2 p4 p5)
  (let ([p1 (hc-append p1 p5 p2 p5)]
        [p2 (hc-append p4 p2 p1 p4)])
    (hc-append p1 p2)))

(define(body p)
  (vc-append p p))
  
(define (bodycomb p1 p4 p5)
  (let ([p1 (hc-append p5 p4 p1)]
        [p2 (hc-append p5 p1 p4)])
    (hc-append p1 p2)))

(define (bodycomb2 p1 p4 p5)
  (let ([p1 (hc-append p4 p1 p5)]
        [p2 (hc-append p1 p4 p5)])
    (hc-append p1 p2)))

(define (bodycomb3 p1 p4 p5)
  (let ([p1 (hc-append p1 p5 p4)]
        [p2 (hc-append p1 p5 p4)])
    (hc-append p1 p2)))

(define (legcomb p1 p3 p4 p5)
  (let ([p1 (hc-append p4 p1 p4 p3)]
        [p2 (hc-append p3 p5 p1 p5)])
    (hc-append p1 p2)))

(define (legcomb2 p1 p3 p4 p5)
  (let ([p1 (hc-append p5 p4 p1 p3)]
        [p2 (hc-append p3 p5 p4 p1)])
    (hc-append p1 p2)))

(define (legcomb3 p1 p3 p4 p5)
  (let ([p1 (hc-append p1 p5 p4 p3)]
        [p2 (hc-append p3 p4 p5 p1)])
    (hc-append p1 p2)))

(define (alien n color1 color2 color3 color4 color5)
 #|set colors to the particles of creeper and combine body
parts together
|#
  (let* ([eyes1 (colorize (filled-rectangle n n) color1)]
        [eyes2 (colorize (filled-rectangle n n) color2)]
        [eyes3 (colorize (filled-rectangle n n) color4)]
        [eyes4 (colorize (filled-rectangle n n) color5)]
        [e (comb eyes1 eyes2 eyes3 eyes4)]
        [nose1 (colorize (filled-rectangle n n) color1)]
        [nose2 (colorize (filled-rectangle n n) color2)]
        [nose3 (colorize (filled-rectangle n n) color4)]
        [nose4 (colorize (filled-rectangle n n) color5)]
        [nose (ncomb nose1 nose2 nose3 nose4)]
        [mth1 (colorize (filled-rectangle n n) color1)]
        [mth2 (colorize (filled-rectangle n n) color2)]
        [mth3 (colorize (filled-rectangle n n) color4)]
        [m (mcomb mth1 mth2 mth3)]
        [jaw1 (colorize (filled-rectangle n n) color1)]
        [jaw2 (colorize (filled-rectangle n n) color2)]
        [jaw3 (colorize (filled-rectangle n n) color4)]
        [jaw4 (colorize (filled-rectangle n n) color5)]
        [j (jawcomb jaw1 jaw2 jaw3 jaw4)]
        [body1 (colorize (filled-rectangle n n) color1)]
        [body2 (colorize (filled-rectangle n n) color4)]
        [body3 (colorize (filled-rectangle n n) color5)]
        [b (bodycomb body1 body2 body3)]
        [b2 (bodycomb2 body1 body2 body3)]
        [b3 (bodycomb3 body1 body2 body3)]
        [leg1 (colorize (filled-rectangle n n) color1)]
        [leg2 (colorize (filled-rectangle n n) color3)]
        [leg3 (colorize (filled-rectangle n n) color4)]
        [leg4 (colorize (filled-rectangle n n) color5)]
        [l (legcomb leg1 leg2 leg3 leg4)]
        [l2 (legcomb2 leg1 leg2 leg3 leg4)]
        [l3 (legcomb3 leg1 leg2 leg3 leg4)])
    (vc-append (eyes e) nose (mouth m) j (body b) (body b2) (body b3) l l2 l3)))

;(alien 10 "green"  "black" "white" "darkgreen" "lightgreen")

(define (upcomb p1 p2 p3)
  (let ([p1 (hc-append p3 p1 p2 p3)]
        [p2 (hc-append p3 p2 p1 p3)])
    (hc-append p1 p2)))

(define (head p1 p2)
  (let ([p1 (hc-append p1 p2 p1 p2)]
        [p2 (hc-append p2 p1 p2 p1)]
        [p3 (hc-append p1 p2 p1 p2)]
        )
    (vc-append p1 p2 p3)))

(define (list p1 p2 p3)
  (let ([p1 (hc-append p3 p2 p1 p2)]
        [p2 (hc-append p2 p1 p2 p3)])
    (hc-append p1 p2)))

(define (list2 p1 p2 p3)
  (let ([p1 (hc-append p3 p3 p2 p1)]
        [p2 (hc-append p1 p2 p3 p3)])
    (hc-append p1 p2)))

(define (list3 p1 p2 p3)
  (let ([p1 (hc-append p3 p3 p3 p1)]
        [p2 (hc-append p2 p3 p3 p3)])
    (hc-append p1 p2)))

 (define (extra n c1 c2 c3)
 ;creates a pixel heart
   (let*([ht (colorize (filled-rectangle n n) c1)]
         [ht2 (colorize (filled-rectangle n n) c2)]
         [ht3 (colorize (filled-rectangle n n) c3)]
         [h (upcomb ht ht2 ht3)]
         [ht4 (colorize (filled-rectangle n n) c1)]
         [ht5 (colorize (filled-rectangle n n) c2)]
         [h2 (head ht4 ht5)]
         [ht5 (colorize (filled-rectangle n n) c1)]
         [ht6 (colorize (filled-rectangle n n) c2)]
         [ht7 (colorize (filled-rectangle n n) c1)]
         [ht8 (colorize (filled-rectangle n n) c2)]
         [ht9 (colorize (filled-rectangle n n) c3)]
         [h3 (list ht7 ht8 ht9)]
         [ht10 (colorize (filled-rectangle n n) c1)]
         [ht11 (colorize (filled-rectangle n n) c2)]
         [ht12 (colorize (filled-rectangle n n) c3)]
         [h4 (list2 ht10 ht11 ht12)]
         [ht13 (colorize (filled-rectangle n n) c1)]
         [ht14 (colorize (filled-rectangle n n) c2)]
         [ht15 (colorize (filled-rectangle n n) c3)]
         [h5 (list3 ht13 ht14 ht15)])
     (vc-append h (eyes h2) h3 h4 h5)))
; (extra 10 "red" "darkred" "white")