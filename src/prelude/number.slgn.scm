;; Copyright (c) 2013-2014 by Vijay Mathew Pandyalakal, All Rights Reserved.

(define is_number number?)
(define is_integer integer?)
(define is_real real?)
(define is_zero zero?)
(define integer_to_char integer->char)
(define exact_to_inexact exact->inexact)
(define inexact_to_exact inexact->exact)

(define (real_to_integer n)
  (inexact->exact (round n)))

(define (integer_to_real n)
  (exact->inexact n))

(define add +)
(define sub -)
(define mult *)
(define div /)

(define is_number_eq =)
(define is_number_lt <)
(define is_number_gt >)
(define is_number_lteq <=)
(define is_number_gteq >=)
