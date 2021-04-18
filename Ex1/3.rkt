#lang pl

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