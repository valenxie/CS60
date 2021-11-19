#lang racket
(require rackunit)
(provide remove-all)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; remove-all
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; remove-all
;;  inputs: 
;;  output: 
(define (remove-all e L)
;;remove all the e in L, ignore the e in sublists
  (cond
    [(empty? L) empty]
    [(list? (first L)) (cons (first L)(remove-all e (rest L)))]
    [(equal? e (first L))(remove-all e (rest L))]
    [else (cons (first L)(remove-all e (rest L)))]))

; provided tests
(check-equal? (remove-all "i" '("a" "l" "i" "i" "i" "e" "n")) 
              '("a" "l" "e" "n"))
(check-equal? (remove-all "i" '( ("a" "l" "i") "i" "i" "e" "n")) 
              '(("a" "l" "i") "e" "n"))
(check-equal? (remove-all 0 '(1 0 1 0 1 0))  
              '(1 1 1))

; additional tests
(check-equal? (remove-all 0 '())  
              '())
(check-equal? (remove-all "a" '(("a" "l")("i" "e" "n")))  
              '(("a" "l")("i" "e" "n")))
