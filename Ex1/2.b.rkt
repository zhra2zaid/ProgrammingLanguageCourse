#lang pl

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