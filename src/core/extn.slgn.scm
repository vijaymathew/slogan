;; Copyright (c) 2013-2016 by Vijay Mathew Pandyalakal, All Rights Reserved.

(define is_eqv eqv?)
(define is_eq eq?)
(define is_equal equal?)
(define is_boolean boolean?)
(define is_function procedure?)
(define is_symbol symbol?)
(define is_promise ##promise?)

(define (is_true obj) (scm-eq? obj #t))
(define (is_false obj) (scm-eq? obj #f))

(define (is_void obj) (void? obj))
(define (is_object obj) (scm-not (void? obj)))

(define (current_exception_handler) 
  (current-exception-handler))

(define (set_current_exception_handler handler)
  (current-exception-handler handler))

(define is_error error-exception?)
(define error_message error-exception-message)
(define error_args error-exception-parameters)
(define is_noncontinuable_exception noncontinuable-exception?)
(define noncontinuable_exception_reason noncontinuable-exception-reason)

(define (show_exception e #!optional (port (current-output-port)))
  (if (error-exception? e)
      (begin (slgn-display (error-exception-message e) display-string: #t port: port)
             (scm-display #\space port)
             (let loop ((args (error-exception-parameters e)))
               (if (scm-not (null? args))
                   (begin (slgn-display (scm-car args) display-string: #t port: port)
                          (scm-display #\space port)
                          (loop (scm-cdr args))))))
      (display-exception e port)))

(define callcc call/cc)
(define dynamic_wind dynamic-wind)
(define call_with_values call-with-values)

(define (compose #!rest fns)
  (if (null? fns) (set! fns (scm-list identity)))
  (let ((fns (scm-reverse fns)))
    (lambda (#!rest args)
      (let loop ((fns (scm-cdr fns))
                 (result (scm-apply (scm-car fns) args)))
        (if (scm-not (null? fns))
            (loop (scm-cdr fns) ((scm-car fns) result))
            result)))))

(define (mapfn f) (lambda (xs #!rest ys) (scm-apply map f xs ys)))

(define (partial f #!rest args) (lambda (#!rest args2) (scm-apply f (scm-append args args2))))

(define (until p f x) (if (p x) x (until p f (f x))))

(define (complement f) (lambda (#!rest args) (scm-not (scm-apply f args))))

(define (array-accessor&mutator tab)
  (cond
    ((vector? tab) (scm-cons vector-ref vector-set!))
    ((string? tab) (scm-cons string-ref string-set!))
    ((u8vector? tab) (scm-cons u8vector-ref u8vector-set!))
    ((s8vector? tab) (scm-cons s8vector-ref s8vector-set!))
    ((u16vector? tab) (scm-cons u16vector-ref u16vector-set!))
    ((s16vector? tab) (scm-cons s16vector-ref s16vector-set!))
    ((u32vector? tab) (scm-cons u32vector-ref u32vector-set!))
    ((s32vector? tab) (scm-cons s32vector-ref s32vector-set!))
    ((u64vector? tab) (scm-cons u64vector-ref u64vector-set!))
    ((s64vector? tab) (scm-cons s64vector-ref s64vector-set!))
    ((f32vector? tab) (scm-cons f32vector-ref f32vector-set!))
    ((f64vector? tab) (scm-cons f64vector-ref f64vector-set!))
    (else #f)))

(define (list-set-at! xs n v)
  (if (>= n 0)
    (let loop ((ys xs) (n n))
      (cond
        ((null? ys)
          #f)
        ((<= n 0)
          (set-car! ys v))
        (else
          (loop (scm-cdr ys) (- n 1)))))))

(define (map-mutate tab key val)
  (cond
    ((list? tab)
      (if (integer? key)
        (list-set-at! tab key val)
        (let ((entry (scm-assoc key tab)))
          (if entry
            (set-cdr! entry val)
            (set-cdr! tab (scm-cons (scm-cons key val) (scm-cdr tab)))))))
    ((hashtable? tab)
      (hashtable_set tab key val))
    ((%bitvector? tab)
      (if val
        (bitvector-set! tab key)
        (bitvector-clear! tab key)))
    (else
      (let ((aam (array-accessor&mutator tab)))
        (if aam
          ((scm-cdr aam) tab key val)
          (error "Mutator not found for object - " tab))))))

(define (map-access tab key default)
  (cond
    ((list? tab)
      (if (integer? key)
        (with-exception-handler
          (lambda (e) default)
          (lambda () (list-ref tab key)))
        (get key tab default)))
    ((hashtable? tab)
      (hashtable_at tab key default))
    ((%bitvector? tab)
      (if (< key (bitvector-length tab))
        (bitvector-set? tab key)
        default))
    (else
      (let ((aam (array-accessor&mutator tab)))
        (if aam
          (with-exception-handler
            (lambda (e) default)
            (lambda () ((scm-car aam) tab key)))
          (error "Accessor not found for object - " tab))))))

(define (@ tab key #!key (value *void*) (default #f))
  (if (scm-not (scm-eq? value *void*))
    (map-mutate tab key value)
    (map-access tab key default)))

(define (do_times n fn #!key (from 0) init)
  (call/cc
    (lambda (break)
      (if (not (procedure? fn))
        (error "expected procedure instead of " fn))
      (let ((cmpr (if (> n from) < >))
            (trans (if (> n from) + -)))
        (let loop ((i from) (res init))
          (if (cmpr i n)
            (loop (trans i 1) (fn i res break))
            res))))))

(define (not-equal? a b)
  (scm-not (equal? a b)))

(define == equal?)
(define <> not-equal?)
