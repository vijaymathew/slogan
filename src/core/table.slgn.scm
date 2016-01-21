;; Copyright (c) 2013-2016 by Vijay Mathew Pandyalakal, All Rights Reserved.

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

(define (hashtable_contains ht key)
  (let ((table (hashtable-table ht))
        (predic (hashtable-eq-predic ht)))
    (table-search (lambda (k v) (predic k key)) table)))

(define (hashtable_update ht key proc #!optional default)
  (let ((table (hashtable-table ht)))
    (table-set! table key (proc (table-ref table key default)))))

(define (hashtable_size ht) (table-length (hashtable-table ht)))

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
