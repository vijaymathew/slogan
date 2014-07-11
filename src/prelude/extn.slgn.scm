;; Copyright (c) 2013-2014 by Vijay Mathew Pandyalakal, All Rights Reserved.

(define is_eqv eqv?)
(define is_eq eq?)
(define is_equal equal?)
(define is_boolean boolean?)
(define is_function procedure?)
(define is_symbol symbol?)
(define is_promise ##promise?)

(define command_line command-line)

(define (current_exception_handler) 
  (current-exception-handler))

(define (set_current_exception_handler handler)
  (current-exception-handler handler))

(define is_error error-exception?)
(define error_message error-exception-message)
(define error_args error-exception-parameters)
(define display_exception display-exception)

(define callcc call/cc)
(define dynamic_wind dynamic-wind)

(define (compose #!rest fns)
  (let ((fns (reverse fns)))
    (lambda (#!rest args)
      (let loop ((fns (cdr fns))
                 (result (apply (car fns) args)))
        (if (not (null? fns))
            (loop (cdr fns) ((car fns) result))
            result)))))
