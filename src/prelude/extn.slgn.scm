;; Copyright (c) 2013-2014 by Vijay Mathew Pandyalakal, All Rights Reserved.

(define is_eqv eqv?)
(define is_eq eq?)
(define is_equal equal?)
(define is_boolean boolean?)
(define is_function procedure?)
(define is_procedure procedure?)
(define is_symbol symbol?)
(define is_promise ##promise?)

(define (is_true obj) (eq? obj #t))
(define (is_false obj) (eq? obj #f))

(define (is_object obj) (not (void? obj)))

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
             (display #\space port)
             (let loop ((args (error-exception-parameters e)))
               (if (not (null? args))
                   (begin (slgn-display (car args) display-string: #t port: port)
                          (display #\space port)
                          (loop (cdr args))))))
      (display-exception e port)))

(define callcc call/cc)
(define dynamic_wind dynamic-wind)
(define call_with_values call-with-values)

(define (identity x) x)

(define (compose #!rest fns)
  (if (null? fns) (set! fns (list identity)))
  (let ((fns (reverse fns)))
    (lambda (#!rest args)
      (let loop ((fns (cdr fns))
                 (result (apply (car fns) args)))
        (if (not (null? fns))
            (loop (cdr fns) ((car fns) result))
            result)))))

(define (mapfn f) (lambda (xs #!rest ys) (apply map f xs ys)))

(define (partial f #!rest args) (lambda (#!rest args2) (apply f (append args2 args))))

(define (until p f x) (if (p x) x (until p f (f x))))

(define (complement f) (lambda (#!rest args) (not (apply f args))))

(define (safe-equal? a b #!optional final)
  (if (equal? a b) (if final #t b) #f))

(define (safe-num-cmpr a b f final)
  (if (and (number? a)
           (number? b))
      (if (f a b) (if final #t b) #f)
      #f))

(define (safe-< a b #!optional final) (safe-num-cmpr a b < final))
(define (safe-> a b #!optional final) (safe-num-cmpr a b > final))
(define (safe-<= a b #!optional final) (safe-num-cmpr a b <= final))
(define (safe->= a b #!optional final) (safe-num-cmpr a b >= final))

(define (map-mutate tab key val)
  (cond ((list? tab)
         (set-cdr! (assoc key tab) val))
        ((vector? tab)
         (vector-set! tab key val))
        (else
         (hashtable_set tab key val))))

(define (map-access tab key default)
  (cond ((list? tab)
         (get key tab default))
        ((vector? tab)
         (with-exception-handler
          (lambda (e) default)
          (lambda () (vector-ref tab key))))
        (else
         (hashtable_at tab key default))))

(define (@ tab key #!key (value *void*) (default #f))
  (let ((v (map-access tab key default)))
    (if (not (eq? value *void*))
        (map-mutate tab key value))
    v))

      
      
