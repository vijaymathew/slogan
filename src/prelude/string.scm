;; Copyright (c) 2013-2014 by Vijay Mathew Pandyalakal, All Rights Reserved.

(define (string-endswith? s prefix)
  (let ((plen (string-length prefix))
        (slen (string-length s)))
    (if (and (not (zero? plen)) (not (zero? slen)) 
             (<= plen slen))
        (let ((subs (substring s (- slen plen) slen)))
          (string=? subs prefix))
        #f)))

(define (string_eq? c1 c2 #!key (ignore_case #f))
  ((if ignore_case string-ci=? char=?) c1 c2))

(define (string_lteq? c1 c2 #!key (ignore_case #f))
  ((if ignore_case string-ci<=? char<=?) c1 c2))

(define (string_gteq? c1 c2 #!key (ignore_case #f))
  ((if ignore_case string-ci>=? char>=?) c1 c2))

(define (string_lt? c1 c2 #!key (ignore_case #f))
  ((if ignore_case string-ci<? char<?) c1 c2))

(define (string_gt? c1 c2 #!key (ignore_case #f))
  ((if ignore_case string-ci>? char>?) c1 c2))

(define string_ends_with? string-endswith?)
(define string_to_list string->list)
(define string_to_number string->number)
(define string_to_symbol string->symbol)
(define string_append string-append)
(define string_copy string-copy)
(define string_fill string-fill!)
(define string_length string-length)
(define string_at string-ref)
(define string_put string-set!)
(define symbol_to_string symbol->string)
(define string_hash string=?-hash)