#lang racket

;;; Student Name: Sawyer Vaughan
;;;
;;; Check one:
;;; [ ] I completed this assignment without assistance or external resources.
;;; [X] I completed this assignment with assistance from Filippos.
;;;       We worked together in order to figure out how the lookup list was stored and how it is passed around in order to get DEFINE working.
;;;     and/or using these external resources: ___

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
        (list 'ANND (lambda (x y) (and x y))) ;; although these AR built in,
        (list 'ORR (lambda (x y) (or x y)))))   ;; they are not simply names.

(define (LAMBDA? x)
  (and (list? x) (and (= (length x) 3) (eq? (first x) 'LAMBDA))))

(define (DEFINE? x)
  (and (list? x) (and (= (length x) 3) (eq? (first x) 'DEFINE))))

(define (add-to-lookup-helper symbols values lookup-list)
  (if (null? symbols)
    lookup-list
    (append (add-to-lookup-helper
              (rest symbols)
              (rest values)
              (lookup-list-add (first symbols) (first values) lookup-list)))))

(define (calculate-lambda l args)
  ; data should look like '(lambda (x y) (ADD x y) ((x 5))) and '((1 2))
  (calculate (third l) (add-to-lookup-helper (second l) args (fourth l))))

(define (lookup-list-add x y lookup-list)
  (if (assq x lookup-list)
    (map (lambda (lookup) (if (eq? (first lookup) x) (list x y) lookup)) lookup-list)
    (append lookup-list (list (list x y)))))

(define (calculate x lookup-list)
  (if (DEFINE? x)
    ;; (list (list)) because append will remove the top level list, essentially
    (repl (lookup-list-add (second x) (third x) lookup-list))
    (if (LAMBDA? x)
      (list 'LAMBDA (second x) (third x) lookup-list)
      (if (or (number? x) (boolean? x) (null? x))
        x
        (if (list? x)
          (if (eq? (first x) 'IPH)
            (if (calculate (second x) lookup-list)
              (calculate (third x) lookup-list)
              (calculate (fourth x) lookup-list))
            (if (and (list? (first x)) (eq? (first (first x)) 'LAMBDA))
              ;; Assume 2 args for now
              (calculate-lambda (calculate (first x) lookup-list) (list (second x) (third x)))
              (if (assq (first x) operator-list)
                (apply (calculate (first x) lookup-list) (calculate (rest x) lookup-list))
                (cons (calculate (first x) lookup-list) (calculate (rest x) lookup-list)))))
            ;; Must be symbol
          (if (assq x lookup-list)
            (second (assq x lookup-list))
            (if (assq x operator-list)
              (second (assq x operator-list))
              (error "Don't know what to do"))))))))

;; REPL

(define (run-repl)
  (display "welcome to my repl.  type some scheme-ish") (newline)
  (repl '()))

(define (repl lookup-list)
  (display "> ")
  (display (calculate (read) lookup-list)) (newline)
  (repl lookup-list))

(run-repl)
;; Welcome to my repl.
;; Type some scheme-ish at the prompt.
;; Type <return> after each expression:
;; mini-eval>> (ADD 3 (MUL 4 5))
;; 23
;; mini-eval>> (SUB (ADD 3 4) (MUL 5 6))
;; -23
;; mini-eval>> (ANND (GT (ADD 3 4) (MUL 5 6)) (LE (ADD 3 (MUL 4 5)) (SUB 0 (SUB (ADD 3 4) (MUL 5 6)))))
;; #f
;; mini-eval>> (ORR (NOTT (GT (ADD 3 4) (MUL 5 6))) (NEQ (ADD 3 (MUL 4 5)) (SUB 0 (SUB (ADD 3 4) (MUL 5 6)))))
;; #f
;; mini-eval>> (IPH (GT (ADD 3 4) 7) (ADD 1 2) (ADD 1 3))
;; 4

;; DEFINE

;; mini-eval>> (DEFINE x 5)
;; mini-eval>> x
;; 5
;; mini-eval>> (ADD x 3)
;; 8

;; LAMBDA

;; mini-eval>> (LAMBDA (x y) (ADD x y))
;; (LAMBDA (x y) (ADD x y) ())
;; mini-eval>> (DEFINE a 3)
;; mini-eval>> (LAMBDA (x y) (ADD x y))
;; (LAMBDA (x y) (ADD x y) (a 3))
;; mini-eval>> ((LAMBDA (x y) (ADD x y)) 1 2)
;; 3
