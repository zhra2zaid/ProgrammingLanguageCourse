#lang pl

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
