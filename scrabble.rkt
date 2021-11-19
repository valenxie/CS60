#lang racket
(require rackunit)
;; scrabble-tile-bag  
;;   letter tile scores and counts from the game of Scrabble
;;   the counts aren't needed they're obtained from
;;   http://en.wikipedia.org/wiki/Image:Scrabble_tiles_en.jpg
;;
(define scrabble-tile-bag
  '((#\a 1 9) (#\b 3 2) (#\c 3 2) (#\d 2 4) (#\e 1 12)
   (#\f 4 2) (#\g 2 3) (#\h 4 2) (#\i 1 9) (#\j 8 1)
   (#\k 5 1) (#\l 1 4) (#\m 3 2) (#\n 1 6) (#\o 1 8)
   (#\p 3 2) (#\q 10 1)(#\r 1 6) (#\s 1 4) (#\t 1 6)
   (#\u 1 4) (#\v 4 2) (#\w 4 2) (#\x 8 1) (#\y 4 2)
   (#\z 10 1) (#\_ 0 2)) ) 
;; end define scrabble-tile-bag
;;
;; The underscore will be used to represent a blank tile, which is a wild-card


(define (subbag? S B)
;;returns true if list B contains all of the elements in S
    (cond
    [(empty? S)]
    [(empty? B) #f]
    [(member (first S)B)(subbag? (rest S)(remove(first S) B))]
    [else #f]))

; provided tests
(check-equal? (subbag? '()      '(s p a m s))   true)
(check-equal? (subbag? '(s s)   '(s p a m s))   true)
(check-equal? (subbag? '(s m)   '(s p a m s))   true)
(check-equal? (subbag? '(a p)   '(s p a m s))   true)
(check-equal? (subbag? '(a m a) '(s p a m s))   false)
(check-equal? (subbag? '(a s)   '(s a))         true)

; additional tests
(check-equal? (subbag? '(a m)      '())   false)
(check-equal? (subbag? '(a )      '(a s))   true)

(define (score-letter a)
;;returns the score of a letter
  (second(assoc a scrabble-tile-bag)))


(define (score-word L)
 ;;returns the score of a string
  (cond
  [(empty? (string->list L)) 0]
  [(+ (score-letter (first (string->list L))) (score-word (list->string(rest (string->list L)))))]))

; provided tests

(check-equal? (score-letter '#\w) 4)
(check-equal? (score-word "zzz")  30)
(check-equal? (score-word "fortytwo") 17)
(check-equal? (score-word "twelve")  12)


(define (best string score rack WL)
;; function that returns the string with best score in a list of strings
;; when all the characters in WL is included in rack
  (let* ([bestS string]
         [bestScore score])
  (cond
    [(empty? WL) (list bestS bestScore)]
    [(and (subbag? (string->list (first WL)) (string->list rack)) (> (score-word(first WL)) score))
     (best (first WL) (score-word(first WL)) rack (rest WL)) ] 
    [else (best string score rack (rest WL))])))

(define (best-word rack WL)
;; rack is a string; WL is a list of strings
  (cond
    [(best "" 0 rack WL)]
    [else '("" 0)]))

; provided tests
(check-equal? (best-word "academy" '("ace" "ade" "cad" "cay" "day"))  '("cay" 8))
(check-equal? (best-word "appler" (list "peal" "peel" "ape" "paper")) '("paper" 9))
(check-equal? (best-word "paler" (list "peal" "peel" "ape" "paper"))  '("peal" 6))
(check-equal? (best-word "kwyjibo" '("ace" "ade" "cad" "cay" "day"))  '("" 0))
(check-equal? (second (best-word "bcademy" '("ace" "ade" "cad" "cay" "bay"))) 8)

; additional tests
(check-equal? (best-word "academy" '("ac1e" "ad2e" "123" "cay3" "d4ay"))  '("" 0))
(check-equal? (best-word "academyu" '())  '("" 0))







