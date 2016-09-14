#lang racket

;;; Student Name: Sawyer Vaughan
;;;
;;; Check one:
;;; [X] I completed this assignment without assistance or external resources.
;;; [ ] I completed this assignment with assistance from ___
;;;     and/or using these external resources: ___

;;; 1.  Create a calculator that takes one argument: a list that represents an expression.

(define (calculate x)
  (if (number? x)
    x
    (cond [(eq? (first x) 'ADD) (+ (calculate (second x)) (calculate (third x)))]
          [(eq? (first x) 'SUB) (- (calculate (second x)) (calculate (third x)))]
          [(eq? (first x) 'MUL) (* (calculate (second x)) (calculate (third x)))]
          [(eq? (first x) 'DIV) (/ (calculate (second x)) (calculate (third x)))]
          [(eq? (first x) 'GT) (> (calculate (second x)) (calculate (third x)))]
          [(eq? (first x) 'LT) (< (calculate (second x)) (calculate (third x)))]
          [(eq? (first x) 'GE) (>= (calculate (second x)) (calculate (third x)))]
          [(eq? (first x) 'LE) (<= (calculate (second x)) (calculate (third x)))]
          [(eq? (first x) 'EQ) (= (calculate (second x)) (calculate (third x)))]
          [(eq? (first x) 'NEQ) (not (= (calculate (second x)) (calculate (third x))))]
          [(eq? (first x) 'AND) (and (calculate (second x)) (calculate (third x)))]
          [(eq? (first x) 'OR) (or (calculate (second x)) (calculate (third x)))]
          [(eq? (first x) 'NOT) (not (calculate (second x)) (calculate (third x)))]
          [(eq? (first x) 'IPH) (if (calculate (second x))
                                  (calculate (third x))
                                  (calculate (fourth x)))])))

(calculate '(ADD 3 4)) ;; --> 7

;;; 2. Expand the calculator's operation to allow for arguments that are themselves well-formed arithmetic expressions.

(calculate '(ADD 3 (MUL 4 5))) ;; --> 23   ;; what is the equivalent construction using list? (calculate (list 'ADD 3 (list 'MUL 4 5)))
(calculate '(SUB (ADD 3 4) (MUL 5 6))) ;; --> -23

;;; 3. Add comparators returning booleans (*e.g.*, greater than, less than, …).
;; Note that each of these takes numeric arguments (or expressions that evaluate to produce numeric values),
;; but returns a boolean.  We suggest operators `GT`, `LT`, `GE`, `LE`, `EQ`, `NEQ`.

	(calculate '(GT (ADD 3 4) (MUL 5 6))) ;; --> #f
	(calculate '(LE (ADD 3 (MUL 4 5)) (SUB 0 (SUB (ADD 3 4) (MUL 5 6))))) ;; --> #t

;;; 4. Add boolean operations ANND, ORR, NOTT

(calculate '(ANND (GT (ADD 3 4) (MUL 5 6)) (LE (ADD 3 (MUL 4 5)) (SUB 0 (SUB (ADD 3 4) (MUL 5 6)))))) ;; --> #f
(calculate '(NOTT (ANND (GT (ADD 3 4) (MUL 5 6)) (LE (ADD 3 (MUL 4 5)) (SUB 0 (SUB (ADD 3 4) (MUL 5 6))))))) ;; --> #t

;;; 5. Add IPH

(calculate '(IPH (GT (ADD 3 4) 7) (ADD 1 2) (ADD 1 3))) ;; -> 4
