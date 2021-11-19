#lang racket
(require rackunit)
(require "hw1pr2_count1bits.rkt")

; student tests
(check-equal? (count1bits 0) 0)
(check-equal? (count1bits -1) "Error!")

; provided tests
(check-equal? (count1bits 6) 2)
(check-equal? (count1bits 7) 3)
(check-equal? (count1bits 42) 3)