#lang racket
(provide count1bits)

(define (count1bits N)
;function returns the number of 1 bits in binary representation
  (cond
     [(equal? N 0) 0]
     [(negative? N) "Error!"]
     [(odd? N)(add1 (count1bits (quotient N 2)))]
     [(even? N) (count1bits (quotient N 2))])
    )
  
