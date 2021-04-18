#lang pl

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