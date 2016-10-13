#lang racket

;;;;;;;;;;
;;; Day 3 in class work

;;;;;;;;;;
;; 0.  Implement factorial both recursively and tail recursively.
;;     Hint:  The tail recursive version will use a helper function.


;;;;;;;;;;;
;; 1.  Filter is built in to scheme.

;; (filter even? '(1 2 3 4 5 6))  --> '(2 4 6)  ;; using the built-in even?
;; (filter teen? '(21 17 2 13 4 42 2 16 3)) --> '(17 13 16)
                        ;; assuming (define (teen x) (and (<= 13 x) (<= x 19)))))
;; (filter list? '(3 (3 2 1) symbol (4 2) (1 (2) 3)) --> '((3 2 1) (4 2) (1 (2) 3))

;; Implement it anyway.  You might want to call it my-filter?  What arguments does it take?





;;;;;;;;;;;
;; 2.  Map is also built in to scheme.

;; (map double '(1 2 3))  --> '(4 5 6)  ;; assuming (define (double x) (* 2 x))
;; (map incr '(1 2 3)) --> '(2 3 4)     ;; assuming (define (incr x) (+ x 1))
;; (map last '((3 2 1) (4 2) (1 2 3)) --> '(1 2 3)
                                        ;; assuming (define (last lst)
                                        ;;            (if (null? (rest lst))
                                        ;;                (first lst)
                                        ;;                (last (rest lst))))

;; Implement it as well.  You might want to call it my-map.  What arguments does it take?







;;;;;;;;;;;
;; 3.  While we're reimplementing built-ins, implement my-append (just like built in append)
;;     It takes two lists and returns a list containing all of the elements of the originals, in order.
;;     Note that it is purely functional, i.e., it doesn't MODIFY either of the lists that it is passed.

;; (append '(1 2 3) '(4 5 6)) --> '(1 2 3 4 5 6)

;; You might want to draw out the box and pointer structures for the original two lists
;; as well as for the new list.  Confirm with a member of the instructional staff....





;;;;;;;;;;;
;; 4.  zip takes two lists, and returns a list of elements of size two, until one of the lists runs out.

;; (zip '(1 2 3) '(4 5 6)) ;; --> '((1 4) (2 5) (3 6))
;; (zip '(1 2 3) '(a b c d e f g)) ;; --> '((1 a) (2 b) (3 c))

;; Implement `zip`.




;;;;;;;;;;;;
;; 5.  Last built-in (for now):  (my-)reverse.
;;     Takes a list, returns a new list with the elements reversed.

;; (reverse '(1 2 3)) --> '(3 2 1)



;;;;;;;;;;;;
;; More puzzles:
;;
;;  - (count elt lst) returns the number of times that elt appears in lst
;;  - (remove-dups lst) returns a new list that contains the elements of lst but without repeats
;;       (remove-dups '(1 2 3 1 4 5 2 4 6)) --> '(1 2 3 4 5 6)
;;  - reverse reverses a list, but doesn't reverse sublists in a tree.  (Try it and see.)
;;    Write deep-reverse, which reverses all sublists as well.
;;  - Which of these can you implement using tail recursion?

(define (list-length lst)
  (if (null? lst)
    0
    (+ 1 (list-length (rest lst)))))

(define (member? elt lst)
  (if (null? lst)
    #f
    (if (equal? elt (first lst))
      #t
      (member? elt (rest lst)))))

(define (list-length-helper lst count)
  (if (null? lst)
    count
    (list-length-helper (rest lst) (+ 1 count))))

(define (list-length2 lst)
  (list-length-helper lst 0))

(display (member? 'ADD '(1 2 34 5 3 'hi ADD))) (newline)
(display (list-length2 '(1 2 3 4 2 5 1 35 2 3 5 6))) (newline)

(define (fact1 arg)
  (if (= arg 1)
    1
    (* arg (fact1 (- arg 1)))))

(define (fact-helper arg current)
  (if (= arg 1)
    current
    (fact-helper (- arg 1) (* arg current))))

(define (fact2 arg)
  (fact-helper arg 1))

(display (fact1 5)) (newline)
(display (fact2 5)) (newline)

(define (my-filter-helper func lst current)
  (if (null? lst)
    current
    (if (func (first lst))
      (if (null? current)
        (my-filter-helper func (rest lst) (list (first lst)))
	(my-filter-helper func (rest lst) (append current (list (first lst)))))
      (my-filter-helper func (rest lst) current))))

(define (my-filter func lst)
  (my-filter-helper func lst '()))

(display (my-filter even? '(1 2 3 4 5 6))) (newline)

(define (double x) (* 2 x))

(define (my-map-helper func lst current)
  (if (null? lst)
    current
    (if (null? current)
      (my-map-helper func (rest lst) (list (func (first lst))))
      (my-map-helper func (rest lst) (append current (list (func (first lst))))))))

(define (my-map func lst)
  (my-map-helper func lst '()))

(display (my-map double '(1 2 3))) (newline)

(define (my-append lst1 lst2)
  (if (null? lst1)
    (if (null? lst2)
      lst2
      (cons (first lst2) (my-append lst1 (rest lst2))))
    (cons (first lst1) (my-append (rest lst1) lst2))))

(display (my-append '(1 2 3) '(4 5 6))) (newline)

(define (my-zip lst1 lst2)
  (if (null? lst1)
    lst1
    (if (null? lst2)
      lst2
      (cons (list (first lst1) (first lst2)) (my-zip (rest lst1) (rest lst2))))))

(display (my-zip '(1 2 3) '(4 5 6 7 8))) (newline)

(define (my-reverse lst)
  (if (null? lst)
    lst
    (append (my-reverse (rest lst)) (list (first lst)))))

(display (my-reverse '(1 2 3 4 5))) (newline)

(define (count-helper elt lst count)
  (if (null? lst)
    count
    (if (equal? elt (first lst))
      (count-helper elt (rest lst) (+ 1 count))
      (count-helper elt (rest lst) count))))

(define (count elt lst)
  (count-helper elt lst 0))

(define (remove-dups-helper lst current)
  (if (null? lst)
    current
    (if (member? (first lst) current)
      (remove-dups-helper (rest lst) current)
      (if (null? current)
        (remove-dups-helper (rest lst) (list (first lst)))
	(remove-dups-helper (rest lst) (append current (list (first lst))))))))
	
(define (remove-dups lst)
  (remove-dups-helper lst '()))

(display (count 3 '(1 2 3 4 5 34 2 2 3 4 43 2 2 3 34))) (newline)
(display (remove-dups '(1 2 3 4 3 2 34 2))) (newline)

