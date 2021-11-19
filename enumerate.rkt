#lang racket
(require racket/trace)
(require rackunit)

(define (new e L)
;;returns a list of elements with indexes in front
  (cond
    [(empty? L)empty]
    [else (cons (cons e (list(first L)))(new (+ 1 e) (rest L)))]))

(define (enumerate L)
  (new 0 L))
  
;(trace enumerate)

; provided tests
(check-equal? (enumerate '(jan feb mar apr)) '((0 jan) (1 feb) (2 mar) (3 apr)))
(check-equal? (enumerate '(0 I II III IV V VI)) 
                         '((0 0) (1 I) (2 II) (3 III) (4 IV) (5 V) (6 VI)))
(check-equal? (enumerate '())  '())

; additional tests
(check-equal? (enumerate '(1 2 3 4))  '((0 1) (1 2) (2 3) (3 4)))
(check-equal? (enumerate '(a b c d))  '((0 a) (1 b) (2 c) (3 d)))
