;; Copyright (c) 2013-2014 by Vijay Mathew Pandyalakal, All Rights Reserved.

(define (string-startswith? s suffix)
  (let ((slen (string-length suffix))
        (len (string-length s)))
    (if (or (zero? slen) (zero? len) 
            (> slen len))
        #f
        (string=? (substring s 0 slen) suffix))))

(define (string-endswith? s prefix)
  (let ((plen (string-length prefix))
        (slen (string-length s)))
    (if (and (not (zero? plen)) (not (zero? slen)) 
             (<= plen slen))
        (let ((subs (substring s (- slen plen) slen)))
          (string=? subs prefix))
        #f)))

(define (string-indexof s ch)
  (let ((len (string-length s)))
    (let loop ((i 0))
      (cond ((>= i len)
             -1)
            ((char=? ch (string-ref s i))
             i)
            (else (loop (+ i 1)))))))

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

(define (string_foreach s fn)
  (let* ((len (string-length s))
         (result (make-string len)))
    (let loop ((i 0))
      (if (>= i len)
          result
          (begin (string-set! result i (fn (string-ref s i)))
                 (loop (+ i 1)))))))

(define (string_upcase s)
  (string_foreach s char-upcase))
  
(define (string_downcase s)
  (string_foreach s char-downcase))

(define string_starts_with? string-startswith?)
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
(define string_indexof string-indexof)

(define (string_replace_all s fch rch)
  (let* ((len (string-length s))
         (result (make-string len)))
    (let loop ((i 0))
      (cond ((>= i len)
             result)
            ((char=? (string-ref s i) fch)
             (string-set! result i rch)
             (loop (+ i 1)))
            (else 
             (string-set! result i (string-ref s i))
             (loop (+ i 1)))))))
          