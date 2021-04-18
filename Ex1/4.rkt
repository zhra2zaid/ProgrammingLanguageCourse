#lang pl

(: is-odd? : Natural -> Boolean)
;;this method is responsible to know if an input natural number
;;is odd or not
;;the functions works recursively as follows:
 ;; basis: if x is zero then x is even so it returns false
 ;;recursive step: returns true if x is odd if (x-1) is even which checked with is-even? method
;;input x which is Natural Number
;;output true if [[x]] is odd Natural Number
(define (is-odd? x)
 (if (zero? x)
 false
 (is-even? (- x 1))))
(: is-even? : Natural -> Boolean)

;;this method is responsible to know if an input natural number
;;is even or not
;;the functions works recursively as follows:
 ;;basis: if x is zero then x is even so it returns true
 ;;recursive stes : returns true if x is even if (x-1) is odd which checked with is-odd? method
;;input x which is Natural Number
;;output true iff [[x]] is even Natural Number

(define (is-even? x)
 (if (zero? x)
 true
 (is-odd? (- x 1))))

;; tests --- is-odd?/is-even?
(test (not (is-odd? 12)))
(test (is-even? 12))
(test (not (is-odd? 0)))
(test (is-even? 0))
(test (is-odd? 1))
(test (not (is-even? 1)))


;; -----------------------------------------------------------------

(: every? : (All (A) (A -> Boolean) (Listof A) -> Boolean))

;;this function is Generic function that checks if each element of type A is satisfying function of type (A -> Boolean)
;;input: list of type A and pred of type A -> Boolean
;;output: true if  for every x in list, (pred x) -> true

(define (every? pred lst)
 (or (null? lst)
 (and (pred (first lst))
 (every? pred (rest lst)))))
;; An instance of the effectiveness of this polymorphic function
(: all-even? : (Listof Natural) -> Boolean)

;; This one covers the function every? with specific values, and call every? with
;;list of Natural numbers and pred function from (Netural -> Boolean) which checks if
;;in an input x, x is even.
;;output: true if  for every x in list, (pred x) -> true

(define (all-even? lst)
 (every? is-even? lst))

;; new-tests
(test (every? is-odd? null))
(test (every? is-odd? '(1 3 5)))
(test (not (every? is-odd? '(1 3 6))))

;; tests
(test (all-even? null))
(test (all-even? (list 0)))
(test (all-even? (list 2 4 6 8)))
(test (not (all-even? (list 1 3 5 7))))
(test (not (all-even? (list 1))))
(test (not (all-even? (list 2 4 1 6))))


;; -----------------------------------------------------------------
(: every2? : (All (A B) (A -> Boolean) (B -> Boolean) (Listof A) (Listof B) ->
Boolean))

;;Generic function that checks for every i in {1,2} if list_i satisfying pred_i
;;input: lst1 of type B, lst2 of type A, pred2 of type (B -> Boolean) and pred1 of type (A -> Boolean)
;;output: true if for each (x,y) in (lst1,lst2) then (pred1 x,pred2 y) returns true
(define (every2? pred1 pred2 lst1 lst2)
 (or (null? lst1) ;; both lists suppose to have the same length
 (and (pred1 (first lst1))
 (pred2 (first lst2))
 (every2? pred1 pred2 (rest lst1) (rest lst2)))))

;; tests
(test (every2? (lambda (x) #t) (lambda (x) #t) '("Zvi" "Mints") '(1 2 3)))
(test (not (every2? (lambda (x) #t) (lambda (x) #f) '("Zvi" "Mints") '(1 2))))
(test (not (every2?  (lambda (x) #t) (lambda (x) (not (eq? x 2))) '("Zvi" "Mints") '(1 2))))
(test (every2?  (lambda (x) #t) (lambda (x) (not (eq? x 3))) '("Zvi" "Mints") '(1 2)))
