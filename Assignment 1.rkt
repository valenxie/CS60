#lang racket
;; problem 1 a
;;Three conditions are set to count the number of n in list L
(define (count n L)
  (cond
    [(empty? L) 0]
    ;; if list is empty return 0
    [(equal? n (first L)) (+ 1 (count n (rest L)))]
    ;; if n equals the first element of L, count 1 and recurse
    [else
     (count n (rest L))]))
    ;; else, recurse the function on rest of the list

(define (remove n L)
;;Three conditions are set to remove repeated n in list L
  (cond
    [(empty? L) empty]
    ;; if list is empty return empty
    [(equal? n (first L)) (remove n (rest L))]
    ;; if n equals the first element of L, recurse the function on rest of L
    [else
     (cons (first L) (remove n (rest L)))]))
    ;; else, create a list of elements beside n recurse the function on rest of the list

(define (compact L)
  (if (empty? L)
      empty
      (cons (list (first L)(count (first L) L)) (compact (remove (first L)L)))))
    ;;create a list of list through recursion

;; problem 1 b
(define (multiplicity n L)
   (if (empty? L)
      empty   
      (first (assoc n L))))
    ;;Get the first element from a set

;; problem 2 
(define (pd n divisor)
  (if (equal? n 1)
      empty
      (if (equal? (remainder n divisor) 0)
          (cons  divisor (pd (quotient n divisor) divisor))
          (pd n (+ 1 divisor)))))
    ;;if first divisor divides, add to a list, divide n and recurse
    ;;if not, + 1 to current divisor and recurse

             
(define (factor n)
   (if (< n 2)
      empty      
      (compact(pd n 2))))
    ;;divisor will start from 2
  
  
  


  
  
      
  
  

  
        
   
  

  