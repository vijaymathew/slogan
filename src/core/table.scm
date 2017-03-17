;; Copyright (c) 2013-2017 by Vijay Mathew Pandyalakal, All Rights Reserved.

(define-structure hashtable table eq-predic hash-fn)

(define (make_hashtable hash-fn eq-predic #!optional size)
  (make-hashtable (if size (make-table hash: hash-fn size: size test: eq-predic)
                      (make-table hash: hash-fn test: eq-predic))
                  eq-predic hash-fn))

(define (make_eq_hashtable #!optional size) (make_hashtable eq?-hash eq? size))
(define (make_eqv_hashtable #!optional size) (make_hashtable eqv?-hash eqv? size))
(define (make_equal_hashtable #!optional size) (make_hashtable equal?-hash equal? size))

(define symbol_hash symbol-hash)
(define string_hash string=?-hash)
(define string_ci_hash string-ci=?-hash)
(define eq_hash eq?-hash)
(define eqv_hash eqv?-hash)
(define equal_hash equal?-hash)

(define is_hashtable hashtable?)

(define (hashtable_equivalence_function ht) (hashtable-eq-predic ht))
(define (hashtable_hash_function ht) (hashtable-hash-fn ht))

(define (hashtable_set ht key obj) (table-set! (hashtable-table ht) key obj))

(define (hashtable_at ht key #!optional default) (table-ref (hashtable-table ht) key default))

(define scm-hashtable_at hashtable_at)
(define scm-hashtable_set hashtable_set)

(define (hashtable_contains ht key)
  (let ((table (hashtable-table ht))
        (predic (hashtable-eq-predic ht)))
    (table-search (lambda (k v) (predic k key)) table)))

(define scm-hashtable_contains hashtable_contains)

(define (hashtable_update ht key proc #!optional default)
  (let ((table (hashtable-table ht)))
    (table-set! table key (proc (table-ref table key default)))))

(define (hashtable_size ht) (table-length (hashtable-table ht)))

(define scm-hashtable_size hashtable_size)

(define (hashtable_copy ht) (make-hashtable (table-copy (hashtable-table ht))
                                            (hashtable-eq-predic ht) 
                                            (hashtable-hash-fn ht)))

(define (hashtable_keys ht)
  (let ((keys '()))
    (table-for-each (lambda (k v) (set! keys (scm-cons k keys))) (hashtable-table ht))
    (list->vector (scm-reverse keys))))

(define (hashtable_values ht)
  (let ((e '()))
    (table-for-each (lambda (k v) (set! e (scm-cons v e))) (hashtable-table ht))
    (list->vector (scm-reverse e))))

(define (hashtable_entries ht)
  (scm-cons (hashtable_keys ht) (hashtable_values ht)))

(define (hashtable_for_each proc ht)
  (table-for-each proc (hashtable-table ht)))

(define (make-equal-hashtable args)
  (let ((ht (make_equal_hashtable)))
    (let loop ((args args))
      (if (null? args)
        ht
        (let ((a (scm-car args)))
          (hashtable_set ht (scm-car a) (scm-cdr a))
          (loop (scm-cdr args)))))))

(define (hashtable->list ht)
  (table->list (hashtable-table ht)))

;; The set datatype
(define-structure set-type ht)

(define (list->set xs)
  (let ((ht (make-table test: equal?)))
    (let loop ((xs xs))
      (if (null? xs)
          (make-set-type ht)
          (begin (table-set! ht (scm-car xs) #t)
                 (loop (scm-cdr xs)))))))

(define (set->list s)
  (scm-map scm-car (table->list (set-type-ht s))))

(define (set->rlist s) (scm-reverse (set->list s)))

(define set_to_list set->list)

(define (make-set #!rest xs)
  (list->set xs))

(define (set-contains? s x)
  (table-ref (set-type-ht s) x #f))

(define (set-merge predic s1 ss)
  (if (null? ss)
      s1
      (let loop ((s1keys (set->list s1))
                 (rkeys '()))
        (if (null? s1keys)
            (list->set rkeys)
            (loop (scm-cdr s1keys)
                  (let ((x (scm-car s1keys)))
                    (if (for_all predic (scm-map (lambda (s) (set-contains? s x)) ss))
                        (scm-cons x rkeys)
                        rkeys)))))))

(define (hashtable->set ht)
  (list->set (table->list (hashtable-table ht))))

(define (set-length s) (table-length (set-type-ht s)))

(define set_length set-length)

(define (set_difference s1 #!rest ss)
  (set-merge is_false s1 ss))

(define (set_intersection s1 #!rest ss)
  (set-merge is_true s1 ss))

(define (set_union s1 #!rest ss)
  (list->set (scm-apply scm-append (set->list s1) (scm-map set->list ss))))

(define (is_superset s1 s2)
  (let ((orig-s1keys (set->list s1)))
    (let loop ((s1keys orig-s1keys)
               (s2keys (set->list s2)))
      (cond ((null? s2keys)
             #t)
            ((null? s1keys)
             #f)
            ((scm-member (scm-car s2keys) orig-s1keys)
             (loop (scm-cdr s1keys) (scm-cdr s2keys)))
            (else #f)))))

(define (is_subset s1 s2) (is_superset s2 s1))

(define is_set set-type?)
(define make_set make-set)
(define is_set_member set-contains?)

(define (set obj)
  (cond ((set-type? obj)
         obj)
        ((hashtable? obj)
         (hashtable->set obj))
        ((list? obj)
         (list->set obj))
        ((vector? obj)
         (list->set (vector->list obj)))
        ((string? obj)
         (list->set (string->list obj)))
        (else
         (scm-error "Cannot convert object to set."))))
