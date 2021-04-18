#lang pl

;;since we want to return a string i gecide to solve this problem
;;by converting the charachters it recieves to string and display it 

(: append5 : Char Char Char Char Char -> String)
(define(append5 arg1 arg2 arg3 arg4 arg5)
  (define c1 (string arg1 arg2 arg3 arg4 arg5))
  c1)
  

 (test (append5 #\e #\d #\c #\b #\a) => "edcba")
 (test (append5 #\a #\p #\p #\l #\e) => "apple")
 (test (append5 #\1 #\2 #\3 #\4 #\5) => "12345")
 

