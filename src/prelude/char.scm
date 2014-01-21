(define (char_eq? c1 c2 #!key (ignore_case #f))
  ((if ignore_case char-ci=? char=?) c1 c2))

(define (char_lteq? c1 c2 #!key (ignore_case #f))
  ((if ignore_case char-ci<=? char<=?) c1 c2))

(define (char_gteq? c1 c2 #!key (ignore_case #f))
  ((if ignore_case char-ci>=? char>=?) c1 c2))

(define (char_lt? c1 c2 #!key (ignore_case #f))
  ((if ignore_case char-ci<? char<?) c1 c2))

(define (char_gt? c1 c2 #!key (ignore_case #f))
  ((if ignore_case char-ci>? char>?) c1 c2))

(define char_alphabetic? char-alphabetic?)
(define char_numeric? char-numeric?)
(define char_whitepsace? char-whitespace?)
(define char_upper_case? char-upper-case?)
(define char_lower_case? char-lower-case?)
(define char_to_integer char->integer)
(define integer_to_char integer->char)
(define char_upcase char-upcase)
(define char_downcase char-downcase)
