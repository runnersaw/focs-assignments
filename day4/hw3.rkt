#lang racket

;;; Student Name: Sawyer Vaughan
;;;
;;; Check one:
;;; [X] I completed this assignment without assistance or external resources.
;;; [ ] I completed this assignment with assistance from ___
;;;     and/or using these external resources: ___

;;;;;;;;;;;
;; 1. assq

;; `assq` is a function that takes a key and an association list.
;;
;; It returns the corresponding key/value pair from the list
;; (*i.e.*, the pair whose key is *eq?* to the one it is given).
;;
;; If the key is not found in the list, `assq` returns `#f`.


(define (my-assq symb lst)
  (if (null? lst)
  	#f
  	(if (eq? (first (first lst)) symb)
  	  (first lst)
  	  (my-assq symb (rest lst)))))

(define operator-list
  (list (list 'ADD +)
        (list 'SUB -)
        (list 'MUL *)
        (list 'DIV /)
        (list 'GT >)
        (list 'LT <)
        (list 'GE >=)
        (list 'LE <=)
        (list 'EQ =)
        (list 'NEQ (lambda (x y) (not (= x y))))
        (list 'ANND (lambda (x y) (and x y)))
        (list 'ORR (lambda (x y) (or x y)))
        (list 'NOTT not)))

(display (my-assq 'ADD operator-list)) (newline) ;; --> '(ADD #<procedure:+>)
(display (my-assq 'ANND operator-list)) (newline) ;; --> '(ANND #<procedure>)
(display (my-assq 'FOO operator-list)) (newline) ;; --> #f

;;;;;;;;;;;
;; 2. lookup-list

;; Add the ability to look up symbols to your evaluator.
;;
;; Add the `lookup-list` argument to your hw2 evaluator (or ours, from the solution set).
;; `(evaluate 'foo lookup-list)` should return whatever `'foo` is associated with in `lookup-list`.

(define (calculate x lookup-list)
  (if (or (number? x) (boolean? x))
    x
    (if (assq x lookup-list)
      (second (assq x lookup-list))
      (cond [(eq? (first x) 'ADD) (+ (calculate (second x) lookup-list) (calculate (third x) lookup-list))]
            [(eq? (first x) 'SUB) (- (calculate (second x) lookup-list) (calculate (third x) lookup-list))]
            [(eq? (first x) 'MUL) (* (calculate (second x) lookup-list) (calculate (third x) lookup-list))]
            [(eq? (first x) 'DIV) (/ (calculate (second x) lookup-list) (calculate (third x) lookup-list))]
            [(eq? (first x) 'GT) (> (calculate (second x) lookup-list) (calculate (third x) lookup-list))]
            [(eq? (first x) 'LT) (< (calculate (second x) lookup-list) (calculate (third x) lookup-list))]
            [(eq? (first x) 'GE) (>= (calculate (second x) lookup-list) (calculate (third x) lookup-list))]
            [(eq? (first x) 'LE) (<= (calculate (second x) lookup-list) (calculate (third x) lookup-list))]
            [(eq? (first x) 'EQ) (= (calculate (second x) lookup-list) (calculate (third x) lookup-list))]
            [(eq? (first x) 'NEQ) (not (= (calculate (second x) lookup-list) (calculate (third x) lookup-list)))]
            [(eq? (first x) 'ANND) (and (calculate (second x) lookup-list) (calculate (third x) lookup-list))]
            [(eq? (first x) 'ORR) (or (calculate (second x) lookup-list) (calculate (third x) lookup-list))]
            [(eq? (first x) 'NOTT) (not (calculate (second x) lookup-list))]
            [(eq? (first x) 'IPH) (if (calculate (second x) lookup-list)
                                    (calculate (third x) lookup-list)
                                    (calculate (fourth x) lookup-list))]))))

(display (calculate '(ADD x (SUB y z)) '((x 3) (y 12) (z 2)))) (newline)

