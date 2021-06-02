#lang pl

#| Please complete the missing rules below  
<SOL> :: = { <NumList> }
        |  { scalar-mult <num> <SOL> };; i had to fill in this
        |  { intersect <SOL> <SOL>} ;; i had to fill in this too
        |  { union <SOL> <SOL>  } 
        |  <id>
        |  { with {<id> <SOL> } <SOL> } ;; this should be a syntactic sugar
       
<NumList> :: =  λ | list of <num> ;; where λ stands for the empty word, i.e., { } is the empty set

;; where <num> is any expression identified by Racket as a Number
;; and <id> is any expression such that Racket identifies '<id> as a symbol
 
|#


;; -----------------------------------------------------
;; The abstract syntax tree SOL
(define-type SET = (Listof Number))
(define-type SOL
  ;; Please complete the missing parts -- you are NOT allowed to use additional variants (constructors)
  [Set  SET]
  [Smult Number SOL]
  [Inter SOL SOL]
  [Union SOL SOL]
  [IdS    Symbol]
  [WithS  Symbol SOL SOL])


;; ---------------------------------------------------------
;; Parser
;; Please complete the missing parts, and add comments (comments should specify 
;; choices you make, and also describe your work process). Keep your code readable. 
(: parse-sexprS : Sexpr -> SOL)
;; to convert s-expressions into SOLs

;; it was easy to write the parser especially that we did like it before (its the same concept we just changed the funcs we use )

(define (parse-sexprS sexpr)
  (match sexpr
    [(list (number: ns) ...) (Set ns) ] ;;fill in here ????
    [(symbol: name) (IdS name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (WithS name (parse-sexprS named) (parse-sexprS body))]  
       [else (error 'parse-sexprS "bad `with' syntax in ~s" sexpr)])]
    [(list 'scalar-mult (number: sc) rhs)(Smult sc (parse-sexprS rhs))]
    [(list 'intersect lhs rhs) (Inter (parse-sexprS lhs) (parse-sexprS rhs))]
    [(list 'union lhs rhs) (Union (parse-sexprS lhs) (parse-sexprS rhs))]
    
    [else (error 'parse-sexprS "bad syntax in ~s" sexpr)]))


(: parseS : String -> SOL)
;; parses a string containing a SOL expression to a SOL AST
(define (parseS str)
  (parse-sexprS (string->sexpr str)))

  
(test (parseS "{1 3  4 1 4  4 2 3 4 1 2 3}") => (Set '(1 3 4 1 4 4 2 3 4 1 2 3)))
(test (parseS "{union {1 2 3} {4 2 3}}") => (Union (Set '(1 2 3)) (Set '(4 2 3))))
(test (parseS "{intersect {1 2 3} {4 2 3}}") => (Inter (Set '(1 2 3)) (Set '(4 2 3))))
(test (parseS "{with S {intersect {1 2 3} {4 2 3}}
                 {union S S}}")
      =error> "parse-sexprS: bad `with' syntax in")




;; ----------------------------------------------------
;; Operations on SETs
;; Please complete the missing parts, and add comments (comments should specify 
;; the role of each procedure, but also describe your work process). Keep your code readable. 


;; we were asked to complete this function
; it was easy to solve using the member command
(: ismember? : Number SET  -> Boolean)
(define (ismember? n l)
  (if [member n l] #t #f) )

(test (ismember? 1 '(3 4 5)) => #f)
(test (ismember? 1 '()) => #f)
(test (ismember? 1 '(1)) => #t)




;; we were asked to complete this function too
;; this one took me a bit more time to implement it took me almost 30 minuates 
(: remove-duplicates : SET  -> SET) 
(define (remove-duplicates l)  
  (cond ((null? l)
         '())
        ((member (first l) (rest l))
         (remove-duplicates (rest l)))
        (else
         (cons (first l) (remove-duplicates (rest l))))))

(test (remove-duplicates '(3 4 5 1 3 4)) => '(5 1 3 4))
(test (remove-duplicates '(1)) => '(1))
(test (remove-duplicates '()) => '())



;; this is the third function we were asked to write
;;since the remove duplicate function was ready so this one took less than 5 minutates to write
(: create-sorted-set : SET -> SET)
(define (create-sorted-set l)
 (remove-duplicates (sort l <)))


(test (create-sorted-set '(3 4 5 3)) => '(3 4 5))
(test (create-sorted-set '(3 2 3 5 6)) => '(2 3 5 6))
(test (create-sorted-set '()) => '())



;;4th function to complete
;;using the create sorted set function and append command this one too tool me less than 10 minuates to write
;; the minuate i understood and get tje idea how to write it and what to use 
(: set-union : SET SET -> SET) 
(define (set-union A B)
   (create-sorted-set (append A B)))


(test (set-union '(3 4 5) '(3 4 5)) => '(3 4 5))
(test (set-union '(3 4 5) '()) => '(3 4 5))
(test (set-union '(3 4 5) '(1)) => '(1 3 4 5))




;;5th function to complete
;; this one was a bit hard so i wrote it my way
(: set-intersection : SET SET -> SET) 
(define set-intersection
  (lambda (s1 s2)
    (cond ((null? s1) '())
          ((member (car s1) s2)
           (cons (car s1)
                 (set-intersection (cdr s1) s2)))
          (else (set-intersection (cdr s1) s2)))))

  
#|
I HAVE TO ANNONATE THAT I DIDNOT KNEW HOW TO ANSWER THE QUESTION AS YOU ASKED TO SO I WROTE ON MY WAY
AND IT DOES THE QUESTION GOAL AND WORKS GOOD
|#


(test (set-intersection '(3 4 5) '(3 4 5)) => '(3 4 5))
(test (set-intersection '(3 4 5) '(3)) => '(3))
(test (set-intersection '(3 4 5) '(1)) => '())




;;6th function to complete
;; this one took me 15 minuates it wasnt that hard
(: set-smult : Number (Listof Number) -> SET)
(define (set-smult n l)        
   (if (null? l)
      '()
      (cons (* (car l) n)
            (set-smult n (cdr l)))))


(test (set-smult 3 '(3 4 5)) => '(9 12 15))
(test (set-smult 2 '()) => '())
;; (test (set-smult 0 '(3 4 5)) => '(0 0 0)) ;; This test is invalid. We assume the scalar is >=1





;;-----------------------------------------------------
;; Substation 
#|
------------------------------------------------------
 Formal specs for `subst':
   (`set' is a { <NumList> }, E, E1, E2 are <SOL>s, `x' is some <id>,
   `y' is a *different* <id>)
      set[v/x]              = set 
      {smult n E}[v/x]      = {smult n E[v/x]}
      {inter E1 E2}[v/x]    = {inter E1[v/x] E2[v/x]}
      {union E1 E2}[v/x]    = {union E1[v/x] E2[v/x]}
      y[v/x]                = y
      x[v/x]                = v
      {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
      {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
|#

;; it was easy to write this  especially that we did like it before
(: substS : SOL Symbol SOL -> SOL)
(define (substS expr from to)
  (cases expr
    [(Set n) expr]
    [(Smult n s) (Smult n (substS s from to))]
    [(Inter l r) (Inter (substS l from to) (substS r from to))]
    [(Union l r) (Union (substS l from to) (substS r from to))]
    [(IdS name) (if (eq? name from) to expr)]
    [(WithS bound-id named-expr bound-body)
     (WithS bound-id
            (substS named-expr from to)
            (if (eq? bound-id from)
                bound-body
                (substS bound-body from to)))]))





;;-----------------------------------------------------
;; Evaluation 
#|
------------------------------------------------------
Evaluation rules:
    eval({ N1 N2 ... Nl })  = sort( create-set({ N1 N2 ... Nl }))
                               ;; where create-set removes all duplications from
                                  the sequence (list) and sort is a sorting procedure
    eval({scalar-mult K E}) = { K*N1 K*N2 ... K*Nl }
                               ;; where eval(E)={ N1 N2 ... Nl }
    eval({intersect E1 E2}) = sort( create-set(set-intersection (eval(E1) , eval(E2)))     
    eval({union E1 E2})     = sort( create-set(set-union (eval(E1) , eval(E2)))
    eval({with {x E1} E2})  = eval(E2[eval(E1)/x])
    eval(id)                = error!
|#



;;---------  the eval procedure ------------------------------
;; Please complete the missing parts, and add comments (comments should specify 
;; the choices you make, and also describe your work process). Keep your code readable. 
(: eval : SOL -> SET)
;; evaluates SOL expressions by reducing them to set values

;; this one too was easy to write this  especially that we did like it before too

(define (eval expr)
  (cases expr
    [(Set S) S]  ;; sort and remove-duplicates
    [(Smult n set) (set-smult n (eval set))]
    [(Inter l r) ( set-intersection (eval l) (eval r))]
    [(Union l r)( set-union (eval l) (eval r))]
    [(WithS name named body)
     (eval (substS body
                   name
                   (Set (eval named))))]
    [(IdS name) (error 'eval "free identifier: ~s" name)]))


(: run : String -> SET)
;; evaluate a SOL program contained in a string
(define (run str)
  (eval (parseS str)))
    
;;(test (run "{1 2 3  4 1 4  4 2 3 4 1 2 3}") => '(1 2 3 4))
(test (run "{union {1 2 3} {4 2 3}}") => '(1 2 3 4))
(test (run "{with {S {intersect {1 2 3} {4 2 3}}}
                 {with {x {4 5 7 6 9 8 8 8}}
                    {union x S}}}")
      => '(2 3 4 5 6 7 8 9))
(test (run "{with {S {intersect {1 2 3} {4 2 3}}}
              {union {scalar-mult 3 B}
                 {4 5 7 9 8 8 8}}}")
      =error> "eval: free identifier:")







;;-------------------------------------------- PART B -----------------------------------------------------------
#| BNF for the WAE language:
       <WAE> ::= <num>
               | { + <WAE> <WAE> }
               | { - <WAE> <WAE> }
               | { * <WAE> <WAE> }
               | { / <WAE> <WAE> }
               | { with { <id> <WAE> } <WAE> }
               | <id>
               | {sqrt <WAE> }
  |#

  ;; WAE abstract syntax trees
(define-type WAE
  [NumW Number]
  [AddW WAE WAE]
  [SubW WAE WAE]
  [MulW WAE WAE]
  [DivW WAE WAE]
  [IdW Symbol]
  [WithW Symbol WAE WAE])



(: parse-sexprW : Sexpr -> WAE) 
(define (parse-sexprW sexpr)
  (match sexpr
    [(number: n) (NumW n)]
    [(symbol: name) (IdW name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (WithW name (parse-sexprW named) (parse-sexprW body))]
       [else (error 'parse-sexprW "bad `with' syntax in ~s" sexpr)])]
    [(list '+ lhs rhs) (AddW (parse-sexprW lhs) (parse-sexprW rhs))]
    [(list '- lhs rhs) (SubW (parse-sexprW lhs) (parse-sexprW rhs))]
    [(list '* lhs rhs) (MulW (parse-sexprW lhs) (parse-sexprW rhs))]
    [(list '/ lhs rhs) (DivW (parse-sexprW lhs) (parse-sexprW rhs))]
    [else (error 'parse-sexprW "bad syntax in ~s" sexpr)]))

(: parseW : String -> WAE)
(define (parseW str)
  (parse-sexprW (string->sexpr str)))



#| Formal specs for `subst':
   (`N' is a <num>, `E1', `E2' are <WAE>s, `x' is some <id>,
   `y' is a different <id>)
      N[v/x]                = N
      {+ E1 E2}[v/x]        = {+ E1[v/x] E2[v/x]}
      {- E1 E2}[v/x]        = {- E1[v/x] E2[v/x]}
      {* E1 E2}[v/x]        = {* E1[v/x] E2[v/x]}
      {/ E1 E2}[v/x]        = {/ E1[v/x] E2[v/x]}
      y[v/x]                = y
      x[v/x]                = v
      {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
      {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
|#



(: substW : WAE Symbol WAE -> WAE)
(define (substW expr from to)
  (cases expr
    [(NumW n) expr]
    [(AddW l r) (AddW (substW l from to) (substW r from to))]
    [(SubW l r) (SubW (substW l from to) (substW r from to))]
    [(MulW l r) (MulW (substW l from to) (substW r from to))]
    [(DivW l r) (DivW (substW l from to) (substW r from to))]
    [(IdW name) (if (eq? name from) to expr)]
    [(WithW bound-id named-expr bound-body)
     (WithW bound-id
           (substW named-expr from to)
           (if (eq? bound-id from)
               bound-body
               (substW bound-body from to)))]))



(: freeInstanceList : WAE -> (Listof Symbol))
;; Description: a function which gets an abstract syntax tree and returns a list of free instances of variables.
;; Input: AST as WAE
;; Output: list of free instances. ( list of Symbols)
(define (freeInstanceList AST)
  (cases AST
    [(NumW  n) '()] ;; there's nothing to do, there is no instances here but we have to return a list so we'll return an empty one.
    [(AddW lhs rhs) (append (freeInstanceList lhs) (freeInstanceList rhs))] ;; find free instances in lhs and rhs.
    [(SubW lhs rhs) (append (freeInstanceList lhs) (freeInstanceList rhs))]
    [(MulW lhs rhs) (append (freeInstanceList lhs) (freeInstanceList rhs))]
    [(DivW lhs rhs) (append (freeInstanceList lhs) (freeInstanceList rhs))]
    [(IdW sym)      (list sym)] ;; THIS IS A FREE INSTANCE! return it as a list as we have to work with lists.
    [(WithW sym named body)  (append (freeInstanceList named) (freeInstanceList (substW body sym (NumW 1))))] )) ;; subsittute all bounded instances so that only free instances would be left to find.

;; tests
;; FreeInstanceList
(test (freeInstanceList (parseW "{with {x 1} {with {y 2} {+ x y}}}")) => '())
(test (freeInstanceList (parseW "{with {x 1} {with {y 2} {+ x z}}}")) => '(z))
(test (freeInstanceList (parseW "{with {x 1} {with {y 2} {+ y z}}}")) => '(z))
(test (freeInstanceList (parseW "{with {x 1} {with {y 2} {+ a z}}}")) => '(a z))
(test (freeInstanceList (parseW "{with {x 1} {with {y b} {+ a z}}}")) => '(b a z))
(test (freeInstanceList (parseW "{with {x 1} {with {y b} {* a z}}}")) => '(b a z))
(test (freeInstanceList (parseW "{with {x 1} {with {y b} {/ a z}}}")) => '(b a z))
(test (freeInstanceList (parseW "w")) => '(w))
(test (freeInstanceList (parseW "{with {xxx 2} {with {yyy 3} {+ {- xx y} z}}}")) => '(xx y z))
(test (freeInstanceList (WithW 'x (NumW 2) (AddW (IdW 'x) (NumW 3)))) => '())
(test (freeInstanceList (parseW "{+ z {+ x z}}")) => '(z x z))
;; parse-sexprW
(test (parse-sexprW (string->sexpr "{+ 1 2}")) => (AddW (NumW 1) (NumW 2)))
(test (parse-sexprW (string->sexpr "{- 1 2}")) => (SubW (NumW 1) (NumW 2)))
(test (parse-sexprW (string->sexpr "{* 1 2}")) => (MulW (NumW 1) (NumW 2)))
(test (parse-sexprW (string->sexpr "{/ 1 2}")) => (DivW (NumW 1) (NumW 2)))
(test (parse-sexprW (string->sexpr "{/ 1 2 3 4}")) =error> "parse-sexprW")
(test (parse-sexprW (string->sexpr "{with / 1 2 3 4}")) =error> "parse-sexprW")
;; substW
(test (substW  (AddW (IdW 'x) (IdW 'x)) 'x (NumW 2)) => (AddW (NumW 2) (NumW 2)))
(test (substW  (WithW 'x (NumW 2) (AddW (IdW 'x) (IdW 'x))) 'x (NumW 3)) => (WithW 'x (NumW 2) (AddW (IdW 'x) (IdW 'x))))


; new tests
(test (freeInstanceList (parseW "{with {x 5} {with {y {/ z 3}} {+ y y}}}")) => '(z))
(test (freeInstanceList(parseW "{with {x g} x}")) => '(g))

;; more tests
(test (freeInstanceList (parseW "w")) => '(w))
(test (freeInstanceList (parseW "{with {xxx 2} {with {yyy 3} {+ {- xx y} z}}}")) => '(xx y z))
(test (freeInstanceList (parseW "{+ z {+ x z}}")) => '(z x z))
(test (freeInstanceList (WithW 'x (NumW 2) (AddW (IdW 'x) (NumW 3)))) => '())
(test (freeInstanceList (parseW "{with {x 1} y}")) => '(y))
(test (freeInstanceList (parseW "{with {x 1} {* x y}}")) => '(y))
(test (freeInstanceList (parseW "{with {x 1} {/ x y}}")) => '(y))
(test (freeInstanceList(parseW "{with {x g} x}")) => '(g))
(test (freeInstanceList(parseW "{with {x g}}")) =error> "bad `with' syntax in")
(test (freeInstanceList(parseW "{ 8 {x g}}")) =error> "bad syntax in")
(test (freeInstanceList (WithW 'x (IdW 'g) (AddW (IdW 'x) (IdW 'x)))) => '(g))
(test (freeInstanceList (parseW "{with { y {/ z 3}  } {+ y y } }")) => '(z))
(test (freeInstanceList (parseW "{with { y {/ z 3}  } {- y y } }")) => '(z))
(test (freeInstanceList (parseW "{with { y {/ z 3}  } {* y y } }")) => '(z))



