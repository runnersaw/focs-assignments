#lang racket

;;; Student Name: Sawyer Vaughan
;;;
;;; Check one:
;;; [ ] I completed this assignment without assistance or external resources.
;;; [X] I completed this assignment with assistance from ___
;;;     and/or using these external resources: stack overflow (How to check list length)

;;     NOTE:  You need not worry about error checking in the programs below.

;; 1.  WRITE SQUARE:  given n, returns n^2.  Hint:  use *
(define (square x)
  (* x x))
  
(display (square 2)) (newline)  ;; -> 4

;; 2.  WRITE is-right-triangle:  given three numbers, returns true iff the third
;;     could be the hypotenuse of a right triangle with the specified three side lengths
;;     Hint:  use = to compare numeric values

(define (is-right-triangle side1 side2 side3)
  (eq? (square side3) (+ (square side1) (square side2))))
  
(display (is-right-triangle 3 4 5)) (newline)  ;; -> #t
(display (is-right-triangle 4 5 6)) (newline)  ;; -> #f

;; 3.  WRITE FACTORIAL:  given n, returns n!
;;     Hint:  recursion is your friend
(define (fact n)
  (if (eq? n 1)
    1
    (* n (fact (- n 1)))))

(display (fact 1)) (newline) ;; -> 1
(display (fact 2)) (newline) ;; -> 2
(display (fact 3)) (newline) ;; -> 6
(display (fact 4)) (newline) ;; -> 24

;; 4.  WRITE FIBONACCI:  given n, returns the nth fibonacci number as shown below
;;     Hint:  don't run this on really big numbers!
(define (fib n)
  (if (eq? n 1)
  	1
  	(begin
	  (if (eq? n 2)
	    1
	  	(+ (fib (- n 1)) (fib (- n 2)))))))

(display (fib 1)) (newline) ;; -> 1
(display (fib 2)) (newline) ;; -> 1
(display (fib 3)) (newline) ;; -> 2
(display (fib 4)) (newline) ;; -> 3
(display (fib 5)) (newline) ;; -> 5
(display (fib 6)) (newline) ;; -> 8

;; 5.  WRITE a procedure that takes a list of numbers and returns the sum of those numbers
;;     Hint:  first, rest, cons
(define (sum lst)
  (if (null? lst)
  	0
  	(+ (first lst) (sum (rest lst)))))

(display (sum '(1 2 3 4))) (newline) ;; -> 10
(display (sum '(1 20 300))) (newline) ;; -> 321

;; 6.  WRITE a procedure that takes a list of numbers and returns the largest one.
;;     While there are solutions using scheme's built-in max, we were actually hoping you'd do something else...
(define (my-max lst)
  (if (<= (length lst) 1)
  	(first lst)
  	(if (> (first lst) (second lst))
  		(my-max (cons (first lst) (list-tail lst 2)))
  		(my-max (cons (second lst) (list-tail lst 2))))))

(display (my-max '(1 10 2 20 3))) (newline) ;; -> 20
(display (my-max '(1 10 2 200 3))) (newline) ;; -> 200
(display (my-max '(1 10 2 2000 3))) (newline) ;; -> 2000
