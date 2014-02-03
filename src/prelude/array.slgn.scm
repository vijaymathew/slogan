;; Copyright (c) 2013-2014 by Vijay Mathew Pandyalakal, All Rights Reserved.

(define (array dim #!key fill)
  (cond ((integer? dim)
         (make-vector dim fill))
        ((list? dim)
         (if (null? (cdr dim))
             (array (car dim) fill: fill)
             (array (car dim) fill: (array (cdr dim) fill: fill))))
        (else (error "invalid array dimension. " dim))))

(define array_length vector-length)
(define array_at vector-ref)
(define array_set vector-set!)
(define array_to_list vector->list)
