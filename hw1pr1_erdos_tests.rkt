#lang racket

(require rackunit)                    ; load the testing framework
(require "hw1pr1_erdos.rkt")    ; load your definitions

;;;;;;;;;;;;;;;;
;; Testing erdos
;;;;;;;;;;;;;;;;

; student tests
(check-equal? (erdos 0) 0)
(check-equal? (erdos -2) 0)

; provided tests
(check-equal? (erdos 84) 42)
(check-equal? (erdos 85) 256)

;;;;;;;;;;;;;;;;;;;;;;
;; Testing erdos-count
;;;;;;;;;;;;;;;;;;;;;;

; student tests
(check-equal? (erdos-count 1) 0)
(check-equal? (erdos-count -1) "Error!")

; provided tests
(check-equal? (erdos-count 26) 10)
(check-equal? (erdos-count 27) 111)
