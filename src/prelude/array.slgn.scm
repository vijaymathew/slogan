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
(define array_copy vector-copy)
(define subarray subvector)
(define array_append vector-append)
(define subarray_fill subvector-fill!)
(define subarray_move subvector-move!)
(define array_shrink vector-shrink!)

(define (array_sort arr #!key (test >) (type 'quick))
  (let ((s (sort (vector->list arr) test: test type: type))
        (len (vector-length arr)))
    (let loop ((s s)
               (i 0))
      (if (null? s)
          arr
          (begin (vector-set! arr i (car s))
                 (loop (cdr s) (+ i 1)))))))
    
