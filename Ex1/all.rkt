#lang pl

;;------------------------------------------------ QUESTION 1.1 --------------------------------------------------

;;since we want to return a string i gecide to solve this problem
;;by converting the charachters it recieves to string and display it 

(: append5 : Char Char Char Char Char -> String)
(define(append5 arg1 arg2 arg3 arg4 arg5)
  (define c1 (string arg1 arg2 arg3 arg4 arg5))
  c1)
  
 (test (append5 #\e #\d #\c #\b #\a) => "edcba")
 (test (append5 #\a #\p #\p #\l #\e) => "apple")
 (test (append5 #\1 #\2 #\3 #\4 #\5) => "12345")



;;----------------------------------------------QUESTION 1.2 ----------------------------------------------------

;; i solved it as the previous question
;; started by converting the chars into strings
;; then putting it all together in a list
;; returning the list

(: permute3 : Char Char Char  -> (Listof String))
(define(permute3 arg1 arg2 arg3)
  (define c1 (string arg1 arg2 arg3))
  (define c2 (string arg1 arg3 arg2))
  (define c3 (string arg2 arg1 arg3))
  (define c4 (string arg2 arg3 arg1))
  (define c5 (string arg3 arg1 arg2))
  (define c6 (string arg3 arg2 arg1))
  (define lst ( list c1 c2 c3 c4 c5 c6))
  lst )

(test (permute3 #\a #\b #\c) => '("abc" "acb" "bac" "bca" "cab" "cba"))
(test (permute3 #\1 #\2 #\3) => '("123" "132" "213" "231" "312" "321"))
(test (permute3 #\a #\b #\c) => '("abc" "acb" "bac" "bca" "cab" "cba"))




;;------------------------------------------------- QUESTION 2.a ----------------------------------------------------

;;in this question we were asked to write a function
;;that calculate the number of lists in a specific list (list s of any type)
;; this question(all question2 ) wasnt easy, it took me almost a day
;; seraching on the internet and recheck the recursion subject again
;; so first I wrote a function that calculate the length of a list
;; that i called calclength wich uses a helper function 
;; (we did an excersise in the class that is too similar to that)

(: count-3lists : (Listof (Listof Any)) -> Natural)
  (define (count-3lists lists)
  ;;this function calculate the length of list of any type without lists
  (: calclength : ( Listof Any ) -> Natural )
  ( define ( calclength lst )
    ;;helper function that clculates the length recursevily
    (: helper : Natural ( Listof Any ) -> Natural )
    ( define ( helper acc lst )     
    (cond
      [( null? lst ) acc]
      [else (helper (+ 1 acc) ( rest lst ))]))
    (helper 0 lst))
   ;; and this main code that gives us what we were asked for
   ;; which is the number of the lists in a list(that it's length is 3)
  (cond
   [(null? lists) 0]
   [(= (calclength (first lists)) 3) (+ 1 (count-3lists(rest lists)))]
   [else (+ 0 (count-3lists(rest lists)))]))

(test (count-3lists '((1 3 4) (() (1 2 3)) ("tt" "Three" 7) (2 4 6 8) (1 2 3))) => 3)
(test (count-3lists '((5 3 4) (() (#\1 #\2 #\3)) ("abc" #\a 7) (5 4 3 2) (1 2 3))) => 3)
(test (count-3lists '((() (#\a #\b "aaa")) )) => 0)




;;------------------------------------------------- QUESTION 2.b ---------------------------------------------------------

;;this question is the same as the previous one
;; but here we were asked to do it using tail-recursion
;; it wasnt too hard because the concept is known
;; and i used the same clclength i wrote in the previous question
;; and we wrote another helper function that i called helper2
;; that does the work with tail recursion

(: count-3lists-tail : (Listof (Listof Any)) -> Natural)

(define (count-3lists-tail lsts)
  (: calclength : ( Listof Any ) -> Natural )
  ( define ( calclength lst )
    ;;helper function that clculates the length recursevily
    (: helper : Natural ( Listof Any ) -> Natural )
    ( define ( helper acc lst )     
    (cond
      [( null? lst ) acc]
      [else (helper (+ 1 acc) ( rest lst ))]))
    (helper 0 lst))
  
 (: helper2 : Natural (Listof (Listof Any)) -> Natural)
  (define (helper2 acc lsts)
  (cond
   [(null? lsts) acc]
   [(= (calclength (first lsts)) 3) (helper2 (+ 1 acc) (rest lsts))]
   [else (helper2 acc (rest lsts))])) 
   (helper2 0 lsts))

;; also i used the same tets from the previous question
(test (count-3lists-tail '((1 3 4) (() (1 2 3)) ("tt" "Three" 7) (2 4 6 8) (1 2 3))) => 3)
(test (count-3lists-tail '((5 3 4) (() (#\1 #\2 #\3)) ("abc" #\a 7) (5 4 3 2) (1 2 3))) => 3)
(test (count-3lists-tail '((() (#\a #\b "aaa")) )) => 0)

;;--------------------------------------------------- QUESTION 2.c ---------------------------------------------------------

;; here we were asked to solve the previous problems recursively
;; this one took the majoratiy of the time i put in this question
;; the thing that wasnt easy was to understand the "levels of nesting").
;; again i used the previous function calclength
;; then recursevely counts the length of each inner list then the whole one
;; it realy wasnt easy

(: count-3listsRec : (Listof Any) -> Natural)
 (define (count-3listsRec lists)
   
  (: calclength : ( Listof Any ) -> Natural )
   ( define ( calclength lst )
    (: helper : Natural ( Listof Any ) -> Natural )
    ( define ( helper acc lst )     
    (cond
      [( null? lst ) acc]
      [else (helper (+ 1 acc) ( rest lst ))]))
    (helper 0 lst))
  ;;and this is the main code of this quesion that
  ;;counts the number of lists of length 3 recursively
  (cond
   [(null? lists) 0]
   [(and (list? (first lists)) (= (calclength (first lists)) 3)) (+ 1 (count-3listsRec (first lists)) (count-3listsRec (rest lists)))]
   [(and (list? (first lists)) (not(= (calclength (first lists)) 3))) (+ (count-3listsRec (first lists)) (count-3listsRec (rest lists)))]
   [else  (count-3listsRec (rest lists))]))

;;also the same tests
(test (count-3listsRec '((1 3 4) (() (1 2 3)) ("tt" "Three" 7) (2 4 6 8) (1 2 3))) => 4)
(test (count-3listsRec '((5 3 4) (() (#\1 #\2 #\3)) ("abc" #\a 7) (5 4 3 2) (1 2 3))) => 4)
(test (count-3listsRec '((() (#\a #\b "aaa")) )) => 1)


;; --------------------------------------------------- QUESTION 3 ------------------------------------------------------------------

;;this one was not that hard and fun to write 

;;; Defenitions
(: search-stack  : (Symbol KeyStack -> (U String False)) )
(: pop-stack : (KeyStack -> (U KeyStack False) ))

;;keyed-stack data structure.
(define-type KeyStack
  [EmptyKS]
  [Push Symbol String KeyStack])

;;search-stack operation
(define (search-stack key stack)
  (cases stack
    [(EmptyKS) #f]
    [(Push symbol string rest) (if (eq? key symbol) string (search-stack key rest))]
    )
)

;;pop-stack operation
(define (pop-stack stack)
 (cases stack
    [(EmptyKS) #f]
    [(Push symbol string rest) rest]
   )
)

;;tests that are in the homweork paper
(test (EmptyKS) => (EmptyKS))
(test (Push 'b "B" (Push 'a "A" (EmptyKS))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))) => (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))))
(test (search-stack 'a (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => "AAA")
(test (search-stack 'c (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => #f)
(test (search-stack 'b (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => "B")
(test (pop-stack (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (pop-stack (EmptyKS)) => #f)



;;----------------------------------------------------------- QUESTION 4 --------------------------------------------------------------

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


