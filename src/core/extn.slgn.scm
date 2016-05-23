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

(define (vector-safe-ref v k d)
  (if (and (>= k 0) (< k (vector-length v)))
      (vector-ref v k)
      d))

(define (string-safe-ref v k d)
  (if (and (>= k 0) (< k (string-length v)))
      (string-ref v k)
      d))

(define (u8vector-safe-ref v k d)
  (if (and (>= k 0) (< k (u8vector-length v)))
      (u8vector-ref v k)
      d))

(define (s8vector-safe-ref v k d)
  (if (and (>= k 0) (< k (s8vector-length v)))
      (s8vector-ref v k)
      d))

(define (u16vector-safe-ref v k d)
  (if (and (>= k 0) (< k (u16vector-length v)))
      (u16vector-ref v k)
      d))

(define (s16vector-safe-ref v k d)
  (if (and (>= k 0) (< k (s16vector-length v)))
      (s16vector-ref v k)
      d))

(define (u32vector-safe-ref v k d)
  (if (and (>= k 0) (< k (u32vector-length v)))
      (u32vector-ref v k)
      d))

(define (s32vector-safe-ref v k d)
  (if (and (>= k 0) (< k (s32vector-length v)))
      (s32vector-ref v k)
      d))

(define (u64vector-safe-ref v k d)
  (if (and (>= k 0) (< k (u64vector-length v)))
      (u64vector-ref v k)
      d))

(define (s64vector-safe-ref v k d)
  (if (and (>= k 0) (< k (s64vector-length v)))
      (s64vector-ref v k)
      d))

(define (f32vector-safe-ref v k d)
  (if (and (>= k 0) (< k (f32vector-length v)))
      (f32vector-ref v k)
      d))

(define (f64vector-safe-ref v k d)
  (if (and (>= k 0) (< k (f64vector-length v)))
      (f64vector-ref v k)
      d))

(define (array-safe-accessor&mutator tab)
  (cond
    ((u8vector? tab) (scm-cons u8vector-safe-ref u8vector-set!))
    ((s8vector? tab) (scm-cons s8vector-safe-ref s8vector-set!))
    ((u16vector? tab) (scm-cons u16vector-safe-ref u16vector-set!))
    ((s16vector? tab) (scm-cons s16vector-safe-ref s16vector-set!))
    ((u32vector? tab) (scm-cons u32vector-safe-ref u32vector-set!))
    ((s32vector? tab) (scm-cons s32vector-safe-ref s32vector-set!))
    ((u64vector? tab) (scm-cons u64vector-safe-ref u64vector-set!))
    ((s64vector? tab) (scm-cons s64vector-safe-ref s64vector-set!))
    ((f32vector? tab) (scm-cons f32vector-safe-ref f32vector-set!))
    ((f64vector? tab) (scm-cons f64vector-safe-ref f64vector-set!))
    (else #f)))

(define (array-accessor&mutator tab)
  (cond
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
   ((vector? tab)
    (vector-set! tab key val))
   ((string? tab)
    (string-set! tab key val))
   ((hashtable? tab)
    (hashtable_set tab key val))   
   ((list? tab)
    (if (integer? key)
        (list-set-at! tab key val)
        (let ((entry (scm-assoc key tab)))
          (if entry
              (set-cdr! entry val)
              (set-cdr! tab (scm-cons (scm-cons key val) (scm-cdr tab)))))))
   ((%bitvector? tab)
    (if val
        (bitvector-set! tab key)
        (bitvector-clear! tab key)))
    (else
     (let ((aam (array-accessor&mutator tab)))
       (if aam
           ((scm-cdr aam) tab key val)
           (error "Mutator not found for object - " tab))))))

(define (map-safe-access tab key default)
  (cond
   ((vector? tab)
    (vector-safe-ref tab key default))
   ((string? tab)
    (string-safe-ref tab key default))
   ((hashtable? tab)
    (hashtable_at tab key default))   
   ((list? tab)
    (get key tab default))
   ((%bitvector? tab)
    (if (< key (bitvector-length tab))
        (bitvector-set? tab key)
        default))
   (else
    (let ((aam (array-safe-accessor&mutator tab)))
      (if aam
          ((scm-car aam) tab key default)
          (error "Accessor not found for object - " tab))))))

(define (ref tab key #!key (value *void*) (default #f))
  (if (scm-not (scm-eq? value *void*))
    (map-mutate tab key value)
    (map-safe-access tab key default)))

(define (map-access tab key)
  (cond
   ((vector? tab)
    (vector-ref tab key))
   ((string? tab)
    (string-ref tab key))
   ((hashtable? tab)
    (hashtable_at tab key))   
   ((list? tab)
    (get key tab))
   ((%bitvector? tab)
    (if (< key (bitvector-length tab))
        (bitvector-set? tab key)
        #f))
   (else
    (let ((aam (array-accessor&mutator tab)))
      (if aam
          ((scm-car aam) tab key)
          (error "Accessor not found for object - " tab))))))

(define (*-@-* tab key #!optional (value *void*))
  (if (scm-not (scm-eq? value *void*))
      (map-mutate tab key value)
      (map-access tab key)))

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
