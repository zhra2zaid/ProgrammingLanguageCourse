#lang pl untyped

;; I have to annotate that in general this assignment wasnt that easy, it was confusing somehow.


;;------------------------------Question1.a-------------------------------------------------
#|
here we were asked to wrew an SE language:

<SE> ::= <string>(1)
         |<SE><SE>  (2)
         |{string-length <string>} (3)
         |{string-append <SE> } (4)
         |{string-insert <SE> }  (5)
         |{number->string <SE> (string-length <SE>)*}  (6)

<string>::= <num>    (7)
           |ג      ;;empty string   (8)
           |<string><string>        (8(2))
           |<char>                  (9)
           |{string <char>* }       (10)
           |<num>*                  (11)
           |"<string>"              (12)
|#



;;------------------------------Question1.b-------------------------------------------------
#|

here i used the examples shown in the homework paper.

example 1 : ( string-append ( string #\1 #\2 #\4 ) "12" )

      4
 <SE> => |{string-append <SE> }
      2
      => |{string-append <SE> <SE> }
      1
      => {string-append <string> <SE>}
      1
      => {string-append <string> <string>}
      10
      => {string-append {string <char>* } <SE>}
      12
      => ( string-append {string <char>* } "<string>" )

      => ( string-append ( string #\1 #\2 #\4 ) "12" )


example 2: ( number->string ( string-length "0033344" ) )

     6
<SE> => {number->string <SE> (string-length <string>)}
     1
     => {number->string <string> (string-length <string>)}
     8
     => {number->string ג(string-length <string>)}
     3
     => ( number->string (string-length <string>) )
     12
     => ( number->string (string-length "<string>") )
    7
     => ( number->string ( string-length "<num>" ) )
    
     => ( number->string ( string-length "0033344" ) )



example 3: ( string-append "" ( string-insert "13" #\4 66 ) "" )

     4
<SE> => {string-append <SE> }
     2
     => {string-append <SE><SE>}
     2
     => {string-append <SE><SE><SE>}
     1
     => {string-append <string><SE><SE>}
     12
     => {string-append "string"<SE><SE>}
     8
     => {string-append "ג"<SE><SE>}
     5
     => {string-append ""(string-insert <SE>)<SE>}
     2*2
    => {string-append ""(string-insert <SE><SE><SE>)<SE>}
    1
    => {string-append ""(string-insert <string><SE><SE>)<SE>}
    12
    => {string-append ""(string-insert "string" <SE><SE>)<SE>}
    8(2)*2
    => {string-append ""(string-insert "string string string" <SE><SE>)<SE>}
    7
    => {string-append ""(string-insert "<unm>" <SE><SE>)<SE>}
    1
    => {string-append ""(string-insert "<unm>" <string><SE>)<SE>}
    9
    => {string-append ""(string-insert "<unm>" <char><SE>)<SE>}
    1
    =>{string-append ""(string-insert "<unm>" <char><string>)<SE>}
    7
    => {string-append ""(string-insert "<unm>" <char><num>)<SE>}
    1
    => {string-append ""(string-insert "<unm>" <char><num>)<string>}
    12
    => {string-append ""(string-insert "<unm>" <char><num>)"string"}
    8
    =>{string-append ""(string-insert "<unm>" <char> <num>)"ג"}

=> ( string-append "" ( string-insert "1357" #\4 66 ) "" )


|#




;;----------------------------------------------Question 2 -------------------------------------------

;; === Higher Order Functions ===

(: sum-of-squares : (Listof Number) -> Number)
(: square : Number -> Number)
#|
this function is responsible to do square operation on an input number
it rwcieves num of type Number and returns the type of it
|#
(define (square num)
  (* num num)
)

;; tests
(test (square 3) => 9)
(test (square 0) => 0)
(test (square -3) => 9)

#|
this function is responsible to aggregate the sum of squares in input list, for each value in the list
the function will square value and combine with '+ and finally returning output of type Number
|#
(define (sum-of-squares list)
  (foldl + 0 (map square list)) 
)

(test (sum-of-squares '(1 2 3)) => 14)
(test (sum-of-squares '()) => 0)
(test (sum-of-squares '(1 1 1)) => 3)
(test (sum-of-squares '(1 2 3 0)) => 14)
(test (sum-of-squares '(-1 -1 -1)) => 3)
(test (sum-of-squares '(-1 -2 -3)) => 14)






#| ---------------------- Question 3.a ------------------------------------------------
here we were requisted to create a polynomial function
as described in the question ,, by just filling the
missing parts of the given code.
it wasn't that easy to understand what each function(define) used for
but when i did,it became much easy to solve.
|#



(: createPolynomial : (Listof Number) -> (Number -> Number))
(define (createPolynomial coeffs)
  ;;helper function (helped from the internet to write it)
  (: poly : (Listof Number) Number Integer Number -> Number)
  (define (poly argsL x power accum)
    (if (null? argsL)
        accum
        (poly (rest argsL) x (+ 1 power) (+ accum
                                            (* (first argsL)
                                               (expt x power))))))
  (: polyX : Number -> Number)
  (define (polyX x)
     (poly coeffs x 0 0))
   polyX)


(define p2345 (createPolynomial '(2 3 4 5)))
(test (p2345 0) => (+ (* 2 (expt 0 0)) (* 3 (expt 0 1)) (* 4 (expt 0 2)) (* 5 (expt 0 3))))
(test (p2345 4) =>(+ (* 2 (expt 4 0)) (* 3 (expt 4 1)) (* 4 (expt 4 2)) (* 5 (expt 4 3))))
(test (p2345 11) => (+ (* 2 (expt 11 0)) (* 3 (expt 11 1)) (* 4 (expt 11 2)) (* 5 (expt 11 3))))

(define p536 (createPolynomial '(5 3 6)))
(test (p536 11) => (+ (* 5 (expt 11 0)) (* 3 (expt 11 1)) (* 6 (expt 11 2))))

(define p_0 (createPolynomial '()))
(test (p_0 4) => 0)

;;---------------------- Question 3.b.i ------------------------------------------------
;; for all question 3.b i used this link: https://pl.barzilay.org/lec03#_bnf,_grammars,_the_ae_language_
;; to understand the subject more and solve the question ;; it was almost as we did in the lecture.

#|
The grammar:
<PLANG> ::= <AEs>
           |<AEs><AEs>
           |{poly <AEs>}

<AEs> ::=<AE>
        |<AE><AE>
        |{+ <AE> <AE>}
        |{- <AE> <AE>}
        |{* <AE> <AE>}
        |{/ <AE> <AE>}

<AE> ::= <AE>
        |{+ <num> <num>}
        |{- <num> <num>}
        |{* <num> <num>}
        |{/ <num> <num>}
|#


;;---------------------- Question 3.b.ii ------------------------------------------------
;;here we wre asked to fill in missing parts of the code.(it was about parsing)
;; it wasnt easy and took me almost an hour to understand the concepts and the issue and get an idea of how to solve it then solving it

(define-type PLANG
 [Poly (Listof AE) (Listof AE)]) ;;we also (Listof AE) 
 (define-type AE
 [Num Number]
 [Add AE AE]
 [Sub AE AE]
 [Mul AE AE]
 [Div AE AE])
 (: parse-sexpr : Sexpr -> AE)
 ;; to convert s-expressions into AEs
 (define (parse-sexpr sexpr)
 (match sexpr
 [(number: n) (Num n)]
 [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
 [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
 [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
 [(list '/ lhs rhs) (Div (parse-sexpr lhs)(parse-sexpr rhs))]
 [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

;; added to convert s-expressions into AEs
(: sexpr->listAE : Sexpr -> (U (Listof AE) AE)) 
(define (sexpr->listAE sexpr)
  (match sexpr [( cons lhs '()) (cons (parse-sexpr lhs) null)]
               [( cons lhs rhs ) (cons (parse-sexpr lhs) (sexpr->listAE rhs))]
               [else (error 'sexpr->listAE "bad syntax in ~s" sexpr)]))


(: parse : String -> PLANG)
 ;; parses a string containing a PLANG expression
 ;;to a PLANG AST
(define (parse str)
  (let ([code (string->sexpr str)])
    (match code [(list (cons 'poly lhs) rhs)
                 (if (null? lhs) (error 'parse "at least one coefficient is required in ~s" code)
                     (if (null? rhs) (error 'parse "at least one point is required in ~s" code)
                         (Poly (sexpr->listAE lhs) (sexpr->listAE rhs))))] [else (error 'parse "bad syntax in ~s" code)]) ))


(test (parse "{{poly 1 2 3} {1 2 3}}") => (Poly (list (Num 1) (Num 2) (Num 3))(list (Num 1) (Num 2) (Num 3))))
(test (parse "{{poly } {1 2} }")=error> "parse: at least one coefficient is required in ((poly) (1 2))")
(test (parse "{{poly 1 2} {} }") =error> "parse: at least one point is required in ((poly 1 2) ())")


;;;;---------------------- Question 3.b.iii ------------------------------------------------
;; this was the most consusing one, i solved it somehow as we did in the lecture it took me almost an hour

(define (eval expr)
(cases expr
[(Num n) n]
[(Add l r) (+ (eval l) (eval r))]
[(Sub l r) (- (eval l) (eval r))]
[(Mul l r) (* (eval l) (eval r))]
[(Div l r) (/ (eval l) (eval r))]))

;;we were asked to implement the eval-poly function
;;im not sure about it !
(: eval-poly : PLANG -> (Listof Number) )
(define (eval-poly p-expr) ;; here we were asked to implement this function and truly it wasnt easy until i understood the eval 
  ( cases p-expr
     [( Poly polynom numbers )
      ( let ([ realNumbers (map (lambda (num)(eval num)) numbers)]
             [ realPolynom (map (lambda (num)(eval num)) polynom)])
         (define temppolynom (createPolynomial realPolynom))
         (map (lambda (num) (temppolynom num))realNumbers))] ))


(: run : String -> (Listof Number))
;; evaluate a FLANG program contained in a string
(define (run str)
(eval-poly (parse str)))

(test (run "{{poly 1 2 3} {1 2 3}}")=> '(6 17 34))
(test (run "{{poly 4 2 7} {1 4 9}}")=> '(13 124 589))
(test (run "{{poly 1 2 3} {1 2 3}}")=> '(6 17 34))
(test (run "{{poly 4/5 } {1/2 2/3 3}}")=> '(4/5 4/5 4/5))
(test (run "{{poly 2 3} {4}}")=> '(14))
(test (run "{{poly 1 1 0} {-1 3 3}}")=> '(0 4 4))










