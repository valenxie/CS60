#lang racket
(provide power)
(provide fast-power)
(require racket/trace)

;; *************************************
;;   Part B - Power
;; *************************************

(define (power base pow)
;returns the base to the power of pow
  (cond
    [(equal? pow 0) 1]
    [(equal? base 0) 0]
    [else
     (* base (power base (- pow 1)))])
  )

;(trace power)

;; *************************************
;;   Part C - Fast-Power
;; *************************************

(define (fast-power base pow)
;calculate the exponent of base in a way that avoid redundant calculations
  (cond
    [(equal? pow 0) 1]
    [(equal? base 0) 0]
    [(odd? pow)(* base (fast-power base (- pow 1)))]
    [(even? pow)(* (fast-power base (/ pow 2))(fast-power base (/ pow 2)))]
    )
  )


