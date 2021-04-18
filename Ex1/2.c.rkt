#lang pl

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