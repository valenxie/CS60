#lang racket

(require rackunit)
(provide prefix?)
(provide sublist?)
(require racket/trace)

(define (prefix? P L)
;; returns true if and only if the list P is identical to
;; the initial elements of the list L
  (cond
    [(empty? P)]
    [(empty? L)#f]
    [(equal? (first P) (first L)) (prefix?(rest P) (rest L))]
    [else #f]))

; provided tests
(check-true  (prefix? '()    '(s p a m)))
(check-true  (prefix? '(s p) '(s p a m)))
(check-false (prefix? '(s m) '(s p a m)))
(check-false (prefix? '(p a) '(s p a m)))

; additional tests
(check-false  (prefix? '(a m)    '()))
(check-true  (prefix? '(a)    '(a p a m)))

(define (sublist? S L)
;;returns true if and only if the list S is identical to
;;some set of consecutive elements of the list L
  (cond
    [(empty? L) #f]
    [(prefix? S L)]
    [else (sublist? S (rest L))]))

;(trace sublist?)

; provided tests
(check-true  (sublist? '()    '(s p a m)))
(check-true  (sublist? '(s p) '(s p a m)))
(check-false (sublist? '(s m) '(s p a m)))
(check-true  (sublist? '(p a) '(s p a m)))

; additional tests
(check-false  (sublist? '(a m)    '()))
(check-true  (sublist? '(a)    '(a p a m)))


  