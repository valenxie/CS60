#lang racket
(require rackunit)
(require racket/trace)

(define (superreverse L)
;; returns a list identical to L, except that all of its top-level lists should be reversed
  (cond
  [(empty? L)empty]
  [else (cons (reverse(first L))(superreverse (rest L)) )]))

; provided tests
(check-equal? (superreverse '( (1 2 3) (4 5 6) (#\k #\o #\o #\l) (#\a #\m) ))
                            '( (3 2 1) (6 5 4) (#\l #\o #\o #\k) (#\m #\a) ) )
(check-equal? (superreverse '( (1 2 3) (4 5 6 (7 8) 9 ) ))
                            '( (3 2 1) (9 (7 8) 6 5 4) ) )

;additional tests
(check-equal? (superreverse '())
                            '())
(check-equal? (superreverse '( (1 2 3) (4 5 6)))
                            '( (3 2 1) (6 5 4)) )


(define (duperreverse L)
;; returns a list whose structure is a complete reversal of L's
  (cond
  [(empty? L)empty]
  [(list?(first L))(append(duperreverse(rest L))(list(duperreverse(first L)))) ]
  [else (append(duperreverse (rest L))(list(first L)))]))

;(trace duperreverse)

;provided tests
(check-equal? (duperreverse '( (1 2 3) (4 5 6) 42 ("k" "o" "o" "l") ("a" "m") ))
    '(  ("m" "a") ("l" "o" "o" "k") 42 (6 5 4) (3 2 1) ) )
(check-equal? (duperreverse '( (1 2 3) (4 5 6 (7 8) 9 ) ))
    '( (9 (8 7) 6 5 4) (3 2 1) ) )

;additional tests
(check-equal? (duperreverse '( (1 2) 4 (5 6) (7 8)))
    '((8 7) (6 5) 4 (2 1)) )
(check-equal? (duperreverse '())
    '())
  