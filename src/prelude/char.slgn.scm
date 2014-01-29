;; Copyright (c) 2013-2014 by Vijay Mathew Pandyalakal, All Rights Reserved.

(define (char_is_eq c1 c2 #!key (ignore_case #f))
  ((if ignore_case char-ci=? char=?) c1 c2))

(define (char_is_lteq c1 c2 #!key (ignore_case #f))
  ((if ignore_case char-ci<=? char<=?) c1 c2))

(define (char_is_gteq c1 c2 #!key (ignore_case #f))
  ((if ignore_case char-ci>=? char>=?) c1 c2))

(define (char_is_lt c1 c2 #!key (ignore_case #f))
  ((if ignore_case char-ci<? char<?) c1 c2))

(define (char_is_gt c1 c2 #!key (ignore_case #f))
  ((if ignore_case char-ci>? char>?) c1 c2))

(define is_char char?)
(define char_is_alphabetic char-alphabetic?)
(define char_is_numeric char-numeric?)
(define char_is_whitepsace char-whitespace?)
(define char_is_upper_case char-upper-case?)
(define char_is_lower_case char-lower-case?)
(define char_to_integer char->integer)
(define integer_to_char integer->char)
(define char_upcase char-upcase)
(define char_downcase char-downcase)
