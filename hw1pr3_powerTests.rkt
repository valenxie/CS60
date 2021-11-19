#lang racket
(require rackunit)
(require "hw1pr3_power.rkt")

; student tests
(check-equal? (power 1 0) 1)
(check-equal? (power 0 1) 0)
(check-equal? (fast-power 1 0) 1)
(check-equal? (fast-power 0 1) 0)


; provided tests
(check-equal? (power 2 10) 1024)
(check-equal? (power 42 10) 17080198121677824)



