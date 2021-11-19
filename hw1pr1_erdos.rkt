#lang racket

;; Two functions will be treated as publicly accessible
;;   (so that we can test them)
(provide erdos erdos-count)

;; erdos: the "3N+1 function"
;;   inputs: an integer, N
;;   output: 3N+1, if N is odd
;;           N/2, if N is even

(define (erdos N)
#|if N is odd, return 3N+1
  if N is even, return N/2|#
  (cond
    [(equal? 0 N) 0]
    [(negative? N) 0]
    [(equal? (remainder N 2) 1) (+ (* 3 N) 1)]
    [(equal? (remainder N 2) 0) (quotient N 2)]))  


(define (erdos-count N)
 ;count numbers of iterations that N needs to get to 1
  (cond
  [(equal? N 1) 0]
  [(negative? N) "Error!"]
  [else
   (add1 (erdos-count(erdos N)))]
  )
)
