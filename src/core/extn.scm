;; Copyright (c) 2013-2017 by Vijay Mathew Pandyalakal, All Rights Reserved.

(c-declare #<<c-declare-end

 #include <time.h>
 #include <stdint.h>
 #include "../include/slogan.h"

 static void get_localtime(___SCMOBJ result, int utc)
 {
   time_t tt;
   struct tm *ltm;
   ___slogan_obj ___temp;

   time(&tt);
   if (utc) ltm = gmtime(&tt);
   else ltm = localtime(&tt);

   ___U32VECTORSET(result, ___fix(0), ___fix(ltm->tm_sec));
   ___U32VECTORSET(result, ___fix(1), ___fix(ltm->tm_min));
   ___U32VECTORSET(result, ___fix(2), ___fix(ltm->tm_hour));
   ___U32VECTORSET(result, ___fix(3), ___fix(ltm->tm_mday));
   ___U32VECTORSET(result, ___fix(4), ___fix(ltm->tm_mon));
   ___U32VECTORSET(result, ___fix(5), ___fix(ltm->tm_year));
   ___U32VECTORSET(result, ___fix(6), ___fix(ltm->tm_wday));
   ___U32VECTORSET(result, ___fix(7), ___fix(ltm->tm_yday));
   ___U32VECTORSET(result, ___fix(8), ___fix(ltm->tm_isdst));
 }

 static void secs_to_tm(___SCMOBJ stt, ___SCMOBJ result)
 {
   float f;
   int itt;
   time_t tt;
   struct tm *ltm;
   ___slogan_obj ___temp;

  ___slogan_obj_to_float(stt, &f);
   itt = (int)f;
   tt = (time_t)itt;
   ltm = localtime(&tt);

   ___U32VECTORSET(result, ___fix(0), ___fix(ltm->tm_sec));
   ___U32VECTORSET(result, ___fix(1), ___fix(ltm->tm_min));
   ___U32VECTORSET(result, ___fix(2), ___fix(ltm->tm_hour));
   ___U32VECTORSET(result, ___fix(3), ___fix(ltm->tm_mday));
   ___U32VECTORSET(result, ___fix(4), ___fix(ltm->tm_mon));
   ___U32VECTORSET(result, ___fix(5), ___fix(ltm->tm_year));
   ___U32VECTORSET(result, ___fix(6), ___fix(ltm->tm_wday));
   ___U32VECTORSET(result, ___fix(7), ___fix(ltm->tm_yday));
   ___U32VECTORSET(result, ___fix(8), ___fix(ltm->tm_isdst));
 }
 
 static int tm_to_secs(___SCMOBJ stm)
 {
   struct tm ltm;
   ltm.tm_sec = ___int(___U32VECTORREF(stm, ___fix(0)));
   ltm.tm_min = ___int(___U32VECTORREF(stm, ___fix(1)));
   ltm.tm_hour = ___int(___U32VECTORREF(stm, ___fix(2)));
   ltm.tm_mday = ___int(___U32VECTORREF(stm, ___fix(3)));
   ltm.tm_mon = ___int(___U32VECTORREF(stm, ___fix(4)));
   ltm.tm_year = ___int(___U32VECTORREF(stm, ___fix(5)));
   ltm.tm_wday = ___int(___U32VECTORREF(stm, ___fix(6)));
   ltm.tm_yday = ___int(___U32VECTORREF(stm, ___fix(7)));
   ltm.tm_isdst = ___int(___U32VECTORREF(stm, ___fix(8)));
   return mktime(&ltm);
 }
 
c-declare-end
)

(define (is_equal x y)
  (if (procedure? x)
      (with-exception-catcher
       (lambda (e)
         (equal? x y))
       (lambda ()
         ((x 'is_equal) y)))
      (equal? x y)))
             
(define is_eqv eqv?)
(define is_eq eq?)
(define is_boolean boolean?)
(define is_function procedure?)
(define is_symbol symbol?)
(define is_promise ##promise?)

(define (is_true obj) (scm-eq? obj #t))
(define (is_false obj) (scm-eq? obj #f))

(define (is_void obj) (void? obj))

(define (current_exception_handler) 
  (current-exception-handler))

(define (set_current_exception_handler handler)
  (current-exception-handler handler))

(define (show_exception e #!optional (port (current-output-port)))
  (if (error-exception? e)
      (let ((args (error-exception-parameters e)))
        (scm-display (error-exception-message e) port)
        (if (scm-not (null? args))
            (begin (scm-print port: port ", ")
                   (let loop ((args args))
                     (if (scm-not (null? args))
                         (begin (scm-display (scm-car args) port)
                                (if (scm-not (null? (scm-cdr args)))
                                    (scm-print port: port ", "))
                                (loop (scm-cdr args))))))))
      (begin (display-exception e port)
             (scm-newline port)
             (slgn-display (parse-exception e) port: port))))

(define scm-show_exception show_exception)

(define (tagged-list tag #!rest elems)
  (scm-cons tag elems))

(define (tagged-list? obj)
  (and (pair? obj) (symbol? (scm-car obj))))

(define (tagged-assoc prop obj)
  (scm-cdr (scm-assoc prop (scm-cdr obj))))

(define (is_error obj)
  (or (error-exception? obj)
      (and (tagged-list? obj)
           (eq? (scm-car obj) 'error))))

(define (error_message obj)
  (if (error-exception? obj)
      (error-exception-message obj)
      (tagged-assoc 'message obj)))

(define (error_args obj)
  (if (error-exception? obj)
      (error-exception-parameters obj)
      (tagged-assoc 'parameters obj)))

(define (is_noncontinuable_exception obj)
  (or (noncontinuable-exception? obj)
      (and (tagged-list? obj)
           (eq? (scm-car obj) 'noncontinuable))))

(define (noncontinuable_exception_reason obj)
  (if (noncontinuable-exception? obj)
      (noncontinuable-exception-reason obj)
      (tagged-assoc 'reason obj)))

(define (trim-args args)
  (let loop ((n 0) (args args) (result '()))
    (if (or (scm-> n 10) (null? args))
        (scm-reverse (scm-cons '... result))
        (loop (scm-+ n 1) (scm-cdr args) (scm-cons (scm-car args) result)))))

(define (parse-exception ex)
  (cond
   ((or (pair? ex) (string? ex) (symbol? ex) (number? ex)) ex)
   ((error-exception? ex)
    (tagged-list 'error
                 (scm-cons 'message (error-exception-message ex))
                 (scm-cons 'parameters (error-exception-parameters ex))))
   ((unbound-global-exception? ex)
    (tagged-list 'unbound_global
                 (scm-cons 'variable (unbound-global-exception-variable ex))
                 (scm-cons 'rte (unbound-global-exception-rte ex))))
   ((range-exception? ex)
    (tagged-list 'range_error
                 (scm-cons 'function (range-exception-procedure ex))
                 (scm-cons 'arguments (range-exception-arguments ex))
                 (scm-cons 'arg_num (range-exception-arg-num ex))))
   ((wrong-number-of-arguments-exception? ex)
    (tagged-list 'wrong_number_of_arguments
                 (scm-cons 'function (wrong-number-of-arguments-exception-procedure ex))
                 (scm-cons 'arguments (wrong-number-of-arguments-exception-arguments ex))))
   ((number-of-arguments-limit-exception? ex)
    (tagged-list 'number_of_arguments_limit
                 (scm-cons 'function (number-of-arguments-limit-exception-procedure ex))
                 (scm-cons 'arguments (trim-args (number-of-arguments-limit-exception-arguments ex)))))
   ((nonprocedure-operator-exception? ex)
    (tagged-list 'not_a_function
                 (scm-cons 'function (nonprocedure-operator-exception-operator ex))
                 (scm-cons 'arguments (nonprocedure-operator-exception-arguments ex))
                 (scm-cons 'rte (nonprocedure-operator-exception-rte ex))))
   ((unknown-keyword-argument-exception? ex)
    (tagged-list 'unknown_keyword_argument
                 (scm-cons 'function (unknown-keyword-argument-exception-procedure ex))
                 (scm-cons 'arguments (unknown-keyword-argument-exception-arguments ex))))
   ((keyword-expected-exception? ex)
    (tagged-list 'keyword_expected
                 (scm-cons 'function (keyword-expected-exception-procedure ex))
                 (scm-cons 'arguments (keyword-expected-exception-arguments ex))))
   ((type-exception? ex)
    (tagged-list 'wrong_type
                 (scm-cons 'function (type-exception-procedure ex))
                 (scm-cons 'arguments (type-exception-arguments ex))
                 (scm-cons 'arg_num (type-exception-arg-num ex))
                 (scm-cons 'type_id (type-exception-type-id ex))))
   ((improper-length-list-exception? ex)
    (tagged-list 'improper_length_list
                 (scm-cons 'function (improper-length-list-exception-procedure ex))
                 (scm-cons 'arguments (improper-length-list-exception-arguments ex))
                 (scm-cons 'arg_num (improper-length-list-exception-arg-num ex))))
   ((divide-by-zero-exception? ex)
    (tagged-list 'divide_by_zero
                 (scm-cons 'function (divide-by-zero-exception-procedure ex))
                 (scm-cons 'arguments (divide-by-zero-exception-arguments ex))))
   ((heap-overflow-exception? ex) (tagged-list 'heap_overflow))
   ((stack-overflow-exception? ex) (tagged-list 'stack_overflow))
   ((os-exception? ex)
    (tagged-list 'system_error
                 (scm-cons 'function (os-exception-procedure ex))
                 (scm-cons 'arguments (os-exception-arguments ex))
                 (scm-cons 'code (os-exception-code ex))
                 (scm-cons 'message (os-exception-message ex))))
   ((no-such-file-or-directory-exception? ex)
    (tagged-list 'no_such_file_or_directory
                 (scm-cons 'function (no-such-file-or-directory-exception-procedure ex))
                 (scm-cons 'arguments (no-such-file-or-directory-exception-arguments ex))))
   ((unbound-os-environment-variable-exception? ex)
    (tagged-list 'unbound_environment_variable
                 (scm-cons 'function (unbound-os-environment-variable-exception-procedure ex))
                 (scm-cons 'arguments (unbound-os-environment-variable-exception-arguments ex))))
   ((scheduler-exception? ex)
    (tagged-list 'scheduler_error (scm-cons 'reason (scheduler-exception-reason ex))))
   ((deadlock-exception? ex) (tagged-list 'deadlock))
   ((abandoned-mutex-exception? ex) (tagged-list 'abandoned_mutex))
   ((join-timeout-exception? ex)
    (tagged-list 'join_timeout
                 (scm-cons 'function (join-timeout-exception-procedure ex))
                 (scm-cons 'arguments (join-timeout-exception-arguments ex))))
   ((started-thread-exception? ex)
    (tagged-list 'started_task
                 (scm-cons 'function (started-thread-exception-procedure ex))
                 (scm-cons 'arguments (started-thread-exception-arguments ex))))
   ((terminated-thread-exception? ex)
    (tagged-list 'terminated_task
                 (scm-cons 'function (terminated-thread-exception-procedure ex))
                 (scm-cons 'arguments (terminated-thread-exception-arguments ex))))
   ((uncaught-exception? ex)
    (tagged-list 'uncaught_error
                 (scm-cons 'function (uncaught-exception-procedure ex))
                 (scm-cons 'arguments (uncaught-exception-arguments ex))
                 (scm-cons 'reason (uncaught-exception-reason ex))))
   ((noncontinuable-exception? ex)
    (tagged-list 'noncontinuable
                 (scm-cons 'reason (noncontinuable-exception-reason ex))))
   ((expression-parsing-exception? ex)
    (tagged-list 'expression_parsing
                 (scm-cons 'kind '(expression-parsing-exception-kind ex))
                 (scm-cons 'parameters (expression-parsing-exception-parameters ex))
                 (scm-cons 'source (expression-parsing-exception-source ex))))
   ((datum-parsing-exception? ex)
    (tagged-list 'datum_parsing
                 (scm-cons 'kind (datum-parsing-exception-kind ex))
                 (scm-cons 'parameters (datum-parsing-exception-parameters ex))
                 (scm-cons 'readenv (datum-parsing-exception-readenv ex))))
   ((cfun-conversion-exception? ex)
    (tagged-list 'cfun_conversion
                 (scm-cons 'function (cfun-conversion-exception-procedure ex))
                 (scm-cons 'arguments (cfun-conversion-exception-arguments ex))
                 (scm-cons 'message (cfun-conversion-exception-message ex))))
   ((sfun-conversion-exception? ex)
    (tagged-list 'sfun_conversion
                 (scm-cons 'function (sfun-conversion-exception-procedure ex))
                 (scm-cons 'arguments (sfun-conversion-exception-arguments ex))
                 (scm-cons 'message (sfun-conversion-exception-message ex))))
   ((multiple-c-return-exception? ex) (tagged-list 'multiple_c_return))
   (else ex)))
        
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
  (if (and (scm->= k 0) (scm-< k (vector-length v)))
      (vector-ref v k)
      d))

(define (string-safe-ref v k d)
  (if (and (scm->= k 0) (scm-< k (string-length v)))
      (string-ref v k)
      d))

(define (u8vector-safe-ref v k d)
  (if (and (scm->= k 0) (scm-< k (u8vector-length v)))
      (u8vector-ref v k)
      d))

(define (s8vector-safe-ref v k d)
  (if (and (scm->= k 0) (scm-< k (s8vector-length v)))
      (s8vector-ref v k)
      d))

(define (u16vector-safe-ref v k d)
  (if (and (scm->= k 0) (scm-< k (u16vector-length v)))
      (u16vector-ref v k)
      d))

(define (s16vector-safe-ref v k d)
  (if (and (scm->= k 0) (scm-< k (s16vector-length v)))
      (s16vector-ref v k)
      d))

(define (u32vector-safe-ref v k d)
  (if (and (scm->= k 0) (scm-< k (u32vector-length v)))
      (u32vector-ref v k)
      d))

(define (s32vector-safe-ref v k d)
  (if (and (scm->= k 0) (scm-< k (s32vector-length v)))
      (s32vector-ref v k)
      d))

(define (u64vector-safe-ref v k d)
  (if (and (scm->= k 0) (scm-< k (u64vector-length v)))
      (u64vector-ref v k)
      d))

(define (s64vector-safe-ref v k d)
  (if (and (scm->= k 0) (scm-< k (s64vector-length v)))
      (s64vector-ref v k)
      d))

(define (f32vector-safe-ref v k d)
  (if (and (scm->= k 0) (scm-< k (f32vector-length v)))
      (f32vector-ref v k)
      d))

(define (f64vector-safe-ref v k d)
  (if (and (scm->= k 0) (scm-< k (f64vector-length v)))
      (f64vector-ref v k)
      d))

(define (scm-sublist ls #!optional (start 0) (end (scm-length ls)))
  (let loop ((ls ls)
             (i 0)
             (result '()))
    (cond ((or (null? ls) (scm-= i end))
           (scm-reverse result))
          ((scm->= i start)
           (loop (scm-cdr ls)
                 (scm-+ i 1)
                 (scm-cons (scm-car ls) result)))
          (else (loop (scm-cdr ls)
                      (scm-+ i 1)
                      result)))))

(define *array-accessors*
  (scm-list (scm-cons 'vector (scm-cons subvector vector-ref))
            (scm-cons 'list (scm-cons scm-sublist list-ref))
            (scm-cons 'string (scm-cons substring string-ref))
            (scm-cons 'bitvector (scm-cons subbitarray bitvector-set?))
            (scm-cons 'u8vector (scm-cons subu8vector u8vector-ref))
            (scm-cons 'u16vector (scm-cons subu16vector u16vector-ref))
            (scm-cons 'u32vector (scm-cons subu32vector u32vector-ref))
            (scm-cons 's8vector (scm-cons subs8vector s8vector-ref))
            (scm-cons 's16vector (scm-cons subs16vector s16vector-ref))
            (scm-cons 's32vector (scm-cons subs32vector s32vector-ref))            
            (scm-cons 'u64vector (scm-cons subu64vector u64vector-ref))
            (scm-cons 's64vector (scm-cons subs64vector s64vector-ref))
            (scm-cons 'f32vector (scm-cons subf32vector f32vector-ref))
            (scm-cons 'f64vector (scm-cons subf64vector f64vector-ref))))
            
(define (array-accessor type tab key)
  (let ((a (scm-assq type *array-accessors*)))
    (if a
        (if (pair? key)
            ((scm-cadr a) tab (scm-car key)
             (let ((end (scm-cdr key)))
               (if (scm-eq? *void* end)
                   (generic-array-length tab)
                                 end)))
            ((scm-cddr a) tab key)))))

(define (map-mutate tab key val)
  (cond
   ((vector? tab)
    (vector-set! tab key val))
   ((string? tab)
    (string-set! tab key val))
   ((hashtable? tab)
    (scm-hashtable_set tab key val))
   ((list? tab)
    (list-set! tab key val))
   ((%bitvector? tab)
    (if val
        (bitvector-set! tab key)
        (bitvector-clear! tab key)))
   ((u8vector? tab)
    (u8vector-set! tab key val))
   ((s8vector? tab)
    (s8vector-set! tab key val))
   ((u16vector? tab)
    (u16vector-set! tab key val))
   ((s16vector? tab)
    (s16vector-set! tab key val))
   ((u32vector? tab)
    (u32vector-set! tab key val))
   ((s32vector? tab)
    (s32vector-set! tab key val))
   ((u64vector? tab)
    (u64vector-set! tab key val))
   ((s64vector? tab)
    (s64vector-set! tab key val))
   ((f32vector? tab)
    (f32vector-set! tab key val))
   ((f64vector? tab)
    (f64vector-set! tab key val))
   (else
    (scm-error 'not_indexed tab))))

(define (map-safe-access tab key default)
  (cond
   ((vector? tab)
    (vector-safe-ref tab key default))
   ((string? tab)
    (string-safe-ref tab key default))
   ((hashtable? tab)
    (scm-hashtable_at tab key default))
   ((list? tab)
    (if (scm-< key (scm-length tab))
        (scm-nth key tab)
        default))
   ((%bitvector? tab)
    (if (scm-< key (bitvector-length tab))
        (bitvector-set? tab key)
        default))
   ((u8vector? tab)
    (u8vector-safe-ref tab key default))
   ((s8vector? tab)
    (s8vector-safe-ref tab key default))
   ((u16vector? tab)
    (u16vector-safe-ref tab key default))
   ((s16vector? tab)
    (s16vector-safe-ref tab key default))
   ((u32vector? tab)
    (u32vector-safe-ref tab key default))
   ((s32vector? tab)
    (s32vector-safe-ref tab key default))
   ((u64vector? tab)
    (u64vector-safe-ref tab key default))
   ((s64vector? tab)
    (s64vector-safe-ref tab key default))
   ((f32vector? tab)
    (f32vector-safe-ref tab key default))
   ((f64vector? tab)
    (f64vector-safe-ref tab key default))
   ((namespace? tab)
    (scm-hashtable_at (namespace-defs tab) key default))
   (else
    (scm-error 'not_indexed tab))))

(define (ref obj key #!optional (default #f))
  (if (procedure? obj)
      ((obj 'ref) key)
      (map-safe-access obj key default)))

(define (ref_set obj key value)
  (if (procedure? obj)
      ((obj 'ref_set) key value)
      (map-mutate obj key value)))

(define (contains obj key)
  (if (procedure? obj)
      ((obj 'contains) key)
      (scm-hashtable_contains obj key)))

(define scm-ref ref)
(define scm-ref_set ref_set)
(define scm-contains contains)

(define (map-access tab key)
  (cond
   ((vector? tab)
    (array-accessor 'vector tab key))
   ((string? tab)
    (array-accessor 'string tab key))
   ((hashtable? tab)
    (scm-hashtable_at tab key))
   ((list? tab)
    (array-accessor 'list tab key))
   ((%bitvector? tab)
    (array-accessor 'bitvector tab key))
   ((u8vector? tab)
    (array-accessor 'u8vector tab key))
   ((s8vector? tab)
    (array-accessor 's8vector tab key))
   ((u16vector? tab)
    (array-accessor 'u16vector tab key))
   ((s16vector? tab)
    (array-accessor 's16vector tab key))
   ((u32vector? tab)
    (array-accessor 'u32vector tab key))
   ((s32vector? tab)
    (array-accessor 's32vector tab key))
   ((u64vector? tab)
    (array-accessor 'u64vector tab key))
   ((s64vector? tab)
    (array-accessor 's64vector tab key))
   ((f32vector? tab)
    (array-accessor 'f32vector tab key))
   ((f64vector? tab)
    (array-accessor 'f64vector tab key))
   ((namespace? tab)
    (scm-hashtable_at (namespace-defs tab) key))
   (else
    (scm-error 'not_indexed tab))))

(define (*-@-* tab key #!optional (value *void*))
  (let ((proc? (procedure? tab)))
    (if (scm-eq? value *void*)
        (if proc?
            ((tab 'ref) key)
            (map-access tab key))
        (begin (if proc?
                   ((tab 'ref_set) key value)
                   (map-mutate tab key value))
               *void*))))

(define (scm-count tab)
  (cond
   ((vector? tab)
    (vector-length tab))
   ((string? tab)
    (string-length tab))
   ((hashtable? tab)
    (scm-hashtable_size tab))
   ((list? tab)
    (scm-length tab))
   ((set-type? tab)
    (set-length tab))
   ((%bitvector? tab)
    (bitvector-length tab))
   ((u8vector? tab)
    (u8vector-length tab))
   ((s8vector? tab)
    (s8vector-length tab))
   ((u16vector? tab)
    (u16vector-length tab))
   ((s16vector? tab)
    (s16vector-length tab))
   ((u32vector? tab)
    (u32vector-length tab))
   ((s32vector? tab)
    (s32vector-length tab))
   ((u64vector? tab)
    (u64vector-length tab))
   ((s64vector? tab)
    (s64vector-length tab))
   ((f32vector? tab)
    (f32vector-length tab))
   ((f64vector? tab)
    (f64vector-length tab))
   (else
    (scm-error 'count_not_supported tab))))

(define (count obj)
  (if (procedure? obj)
      (obj 'count)
      (scm-count obj)))

(define (char-compare x y)
  (cond ((char>? x y) 1)
          ((char<? x y) -1)
          (else 0)))

(define (lex-compare x y lenfn reffn cmprfn)
  (let loop ((len1 (lenfn x))
             (len2 (lenfn y))
             (i 0) (last-cmpr 0))
    (if (scm-not (zero? last-cmpr))
        last-cmpr
        (cond
         ((or (zero? len1) (zero? len2))
          last-cmpr)
         (else
          (loop (scm-- len1 1) (scm-- len2 1)
                (scm-+ i 1) (cmprfn (reffn x i) (reffn y i))))))))

(define (str-compare x y)
  (lex-compare x y string-length string-ref char-compare))

(define (num-compare x y)
  (cond ((scm-< x y) -1)
        ((scm-> x y) 1)
        (else 0)))

(define (slgn-compare x y)
  (cond
   ((number? x)
    (num-compare x y))
   ((string? x)
    (str-compare x y))
   ((char? x)
    (char-compare x y))
   ((symbol? x)
    (str-compare (symbol->string x) (symbol->string y)))
   ((list? x)
    (lex-compare x y scm-length list-ref slgn-compare))
   ((vector? x)
    (lex-compare x y vector-length vector-ref slgn-compare))
   ((%bitvector? x)
    (lex-compare x y bitvector-length bitvector-ref num-compare))
   ((u8vector? x)
    (lex-compare x y u8vector-length u8vector-ref num-compare))
   ((s8vector? x)
    (lex-compare x y s8vector-length s8vector-ref num-compare))    
   ((u16vector? x)
    (lex-compare x y u16vector-length u16vector-ref num-compare))
   ((s16vector? x)
    (lex-compare x y s16vector-length s16vector-ref num-compare))
   ((u32vector? x)
    (lex-compare x y u32vector-length u32vector-ref num-compare))
   ((s32vector? x)
    (lex-compare x y s32vector-length s32vector-ref num-compare))
   ((u64vector? x)
    (lex-compare x y u64vector-length u64vector-ref num-compare))
   ((s64vector? x)
    (lex-compare x y s64vector-length s64vector-ref num-compare))
   ((f32vector? x)
    (lex-compare x y f32vector-length f32vector-ref num-compare))
   ((f64vector? x)
    (lex-compare x y f64vector-length f64vector-ref num-compare))
   (else (scm-error 'compare_not_supported))))

(define (compare x y)
  (if (procedure? x)
      ((x 'compare) y)
      (slgn-compare x y)))

(define scm-compare compare)

(define (lt-compare x y)
  (scm-= (scm-compare x y) -1))

(define (lteq-compare x y)
  (let ((c (scm-compare x y)))
    (or (scm-= c -1) (scm-= c 0))))

(define (gt-compare x y)
  (scm-= (scm-compare x y) 1))

(define (gteq-compare x y)
  (let ((c (scm-compare x y)))
    (or (scm-= c 1) (scm-= c 0))))

(define (scm-str obj)
  (let ((s (open-output-string)))
    (scm-show stream: s obj)
    (let ((r (get-output-string s)))
      (close-port s)
      r)))

(define (to_string obj)
  (if (procedure? obj)
      ((obj 'to_string))
      (scm-str obj)))

(define (do_times n fn #!key (from 0) init)
  (call/cc
    (lambda (break)
      (if (scm-not (procedure? fn))
        (scm-error "expected procedure" fn))
      (let ((cmpr (if (scm-> n from) scm-< scm->))
            (trans (if (scm-> n from) scm-+ scm--)))
        (let loop ((i from) (res init))
          (if (cmpr i n)
            (loop (trans i 1) (fn i res break))
            res))))))

(define scm-is_equal is_equal)

(define (not-equal? a b)
  (scm-not (scm-is_equal a b)))

(define == scm-is_equal)
(define <> not-equal?)

;; time functions
(define get-localtime (c-lambda (scheme-object int) void "get_localtime"))
(define tm->secs (c-lambda (scheme-object) int "tm_to_secs"))
(define secs->tm (c-lambda (scheme-object scheme-object) void "secs_to_tm"))

(define (u32vector->tm vec utc?)
  `((seconds . ,(u32vector-ref vec 0))
    (minute . ,(u32vector-ref vec 1))
    (hour . ,(u32vector-ref vec 2))
    (month_day . ,(u32vector-ref vec 3))
    (month . ,(scm-+ 1 (u32vector-ref vec 4)))
    (year . ,(scm-+ 1900 (u32vector-ref vec 5)))
    (week_day . ,(u32vector-ref vec 6))
    (year_day . ,(u32vector-ref vec 7))
    (is_utc . ,utc?)
    (is_dst . ,(scm-not (zero? (u32vector-ref vec 8))))))

(define (tm->u32vector tm)
  (let ((vec (make-u32vector 9)))
    (u32vector-set! vec 0 (scm-cdr (scm-assq 'seconds tm)))
    (u32vector-set! vec 1 (scm-cdr (scm-assq 'minute tm)))
    (u32vector-set! vec 2 (scm-cdr (scm-assq 'hour tm)))
    (u32vector-set! vec 3 (scm-cdr (scm-assq 'month_day tm)))
    (u32vector-set! vec 4 (scm-- (scm-cdr (scm-assq 'month tm)) 1))
    (u32vector-set! vec 5 (scm-- (scm-cdr (scm-assq 'year tm)) 1900))
    (u32vector-set! vec 6 (scm-cdr (scm-assq 'week_day tm)))
    (u32vector-set! vec 7 (scm-cdr (scm-assq 'year_day tm)))
    (u32vector-set! vec 8 (if (scm-cdr (scm-assq 'is_dst tm)) 1 0))
    vec))

(define (now #!optional secs)
  (let ((result (make-u32vector 9)))
    (if secs
        (secs->tm (scm-floor secs) result)
        (get-localtime result 0))
    (u32vector->tm result #f)))

(define (now_utc)
  (let ((result (make-u32vector 9)))
    (get-localtime result 1)
    (u32vector->tm result #t)))

(define (now_seconds #!optional tm)
  (if (scm-not tm)
      (time->seconds (current-time))
      (tm->secs (tm->u32vector tm))))

(define (tm-comp tm comp)
  (scm-cdr (scm-assq comp tm)))

(define (pad-2 n)
  (if (scm-< n 10)
      (let ((buf (open-output-string)))
        (scm-print port: buf "0" n)
        (get-output-string buf))
      n))

(define (time_to_string tm)
  (let ((buf (open-output-string)))
    (scm-print port: buf (tm-comp tm 'year) "-" (pad-2 (tm-comp tm 'month)) "-" (pad-2 (tm-comp tm 'month_day))
           "T" (pad-2 (tm-comp tm 'hour)) ":" (pad-2 (tm-comp tm 'minute)) ":" (pad-2 (tm-comp tm 'seconds)))
    (if (tm-comp tm 'is_utc)
        (scm-print port: buf "Z"))
    (get-output-string buf)))

(define *mtab* (scm-vector 0 3 2 5 0 3 5 1 4 6 2 4))

(define (day-of-week y m d)
  (let ((y (if (scm-< m 3) (scm-- y 1) y)))
    (scm-remainder (scm-+ (scm-+ (scm-+ (scm-- (scm-+ y (scm-quotient y 4))
                               (scm-quotient y 100))
                            (scm-quotient y 400))
                         (vector-ref *mtab* (scm-- m 1))) d) 7)))

(define (leap-year? y)
  (and (scm->= y 1583)
       (or (and (scm-= 0 (scm-remainder y 4))
                (scm-not (scm-= 0 (scm-remainder y 100))))
           (scm-= 0 (scm-remainder y 400)))))

(define *daytab* (scm-cons (scm-vector 0 31 28 31 30 31 30 31 31 30 31 30 31)
                           (scm-vector 0 31 29 31 30 31 30 31 31 30 31 30 31)))

(define (day-of-year y m d)
  (let ((dt (if (leap-year? y)
                (scm-cdr *daytab*)
                (scm-car *daytab*))))
    (let loop ((i 0) (d d))
      (if (scm-< i m)
          (loop (scm-+ i 1) (scm-+ d (vector-ref dt i)))
          d))))

(define (string_to_time s)
  (let* ((dt (string-split s #\T))
         (ts (scm-cadr dt))
         (len (string-length ts))
         (utc? (char=? #\Z (string-ref ts (scm-- len 1))))
         (ts (if utc? (scm-substring ts 0 (scm-- len 1)) ts))
         (d (string-split (scm-car dt) #\-))
         (t (string-split ts #\:)))
    (let ((m (string->number (list-ref d 1)))
          (y (string->number (list-ref d 0)))
          (d (string->number (list-ref d 2))))
      `((seconds . ,(string->number (list-ref t 2)))
        (minute . ,(string->number (list-ref t 1)))
        (hour . ,(string->number (list-ref t 0)))
        (month_day . ,d)
        (month . ,m)
        (year . ,y)
        (week_day . ,(day-of-week y m d))
        (year_day . ,(day-of-year y m d))
        (is_utc . ,utc?)
        (is_dst . #f)))))

(define process_times process-times)
(define cpu_time cpu-time)
(define real_time real-time)
