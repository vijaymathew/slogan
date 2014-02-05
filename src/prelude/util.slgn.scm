;; Copyright (c) 2013-2014 by Vijay Mathew Pandyalakal, All Rights Reserved.

(define (scm-repr->slgn-repr val)
  (cond ((integer? val)
         val)
        ((and (real? val) (exact? val))
         (exact->inexact val))
        ((or (number? val) 
	     (string? val)
	     (char? val))
         val)
        (else
         (repr-convert val *scheme-reprs*))))

(define (slgn-repr->scm-repr val)
  (repr-convert val *slogan-reprs*))

(define (slgn-directive->scm-directive s)
  (case s
    ((@optional) '#!optional)
    ((@key) '#!key)
    ((@rest) '#!rest)
    (else s)))

(define (slgn-variable->scm-keyword var)
  (string->keyword (symbol->string var)))

(define (slgn-symbol->scm-sym/kw s convfn)
  (let ((str (symbol->string s)))
    (convfn
     (string_replace_all 
      (substring
       str
       1 (string-length str))
      #\_ #\-))))

(define (slgn-symbol->scm-keyword s)
  (slgn-symbol->scm-sym/kw s string->keyword))

(define (slgn-symbol->scm-symbol s)
  (slgn-symbol->scm-sym/kw s string->symbol))

(define (slgn-symbol? s)
  (and (symbol? s)
       (char=? (string-ref (symbol->string s) 0) #\!)))

(define (scm-symbol->slgn-symbol s)
  (let ((str (symbol->string s)))
    (string->symbol (substring str 1 (string-length str)))))

(define (repr-convert val reprs)
  (let ((r (assq val reprs)))
    (if r (cdr r) val)))

(define (void? val) (eq? '#!void val))

(define (slgn-display val #!key display-string (port (current-output-port)))
  (if (not (void? val))
      (cond ((procedure? val)
             (slgn-display-function val port))
            ((list? val)
             (slgn-display-list val port))
            ((pair? val)
             (slgn-display-pair val port))
            ((vector? val)
             (slgn-display-array val port))
            ((char? val)
             (slgn-display-char val port))
            ((string? val)
             (if display-string
                 (display val port)
                 (write val port)))
            ((error-exception? val)
             (display-exception val port))
            (else
             (display (scm-repr->slgn-repr val) port)))))

(define (slgn-display-function proc port)
  (let ((name (with-output-to-string '() 
                                     (lambda () (display proc)))))
    (display "[function " port)
    (let loop ((parts (cddr (string_split name '(#\space #\< #\>)))))
      (if (not (null? parts))
          (begin (display (car parts) port)
                 (if (pair? (cdr parts)) (display #\space port))
                 (loop (cdr parts)))
          (display "]" port)))))
      
(define (slgn-display-list lst port)
  (display "[" port)
  (let loop ((lst lst))
    (cond ((null? lst)
           (display "]" port))
          (else
           (slgn-display (car lst) port: port)
           (if (not (null? (cdr lst)))
               (display ", " port))
           (loop (cdr lst))))))

(define (slgn-display-pair p port)
  (display "[" port)
  (slgn-display (car p) port: port)
  (display " " port)
  (slgn-display (cdr p) port: port)
  (display "]" port))

(define (slgn-display-array a port)
  (display "#" port)
  (slgn-display-list (vector->list a) port))
  
(define (slgn-display-char c port)
  (display "'" port)
  (scm_print port: port c)
  (display "'" port))

(define (display-exception e #!optional (port (current-output-port)))
  (display "error: " port)
  (cond ((error-exception? e)
         (display-error-exception e port))
        ((unbound-global-exception? e)
         (display "unbound global " port) 
         (display (unbound-global-exception-variable e) port))
        ((type-exception? e)
         (display-type-exception e port))
        ((nonprocedure-operator-exception? e)
         (display-nonprocedure-operator-exception e port))
        ((wrong-number-of-arguments-exception? e)
         (display-wrong-number-of-arguments-exception e port))
        ((keyword-expected-exception? e)
         (display-keyword-expected-exception e port))
        ((number-of-arguments-limit-exception? e)
         (display-number-of-arguments-limit-exception e port))
        ((unknown-keyword-argument-exception? e) 
         (display-unknown-keyword-argument-exception e port))
        ((datum-parsing-exception? e)
         (display-datum-parsing-exception e port))
        ((expression-parsing-exception? e)
         (display-expression-parsing-exception e port))
        ((os-exception? e)
         (display-os-exception e port))
        ((no-such-file-or-directory-exception? e)
         (display-no-such-file-or-directory-exception e port))
        ((heap-overflow-exception? e)
         (display "heap overflow." port))
        ((stack-overflow-exception? e)
         (display "stack overflow." port))
        ((range-exception? e)
         (display-range-exception e port))
        ((divide-by-zero-exception? e)
         (display-divide-by-zero-exception e port))
        ((improper-length-list-exception? e)
         (display-improper-length-list-exception e port))
        (else (display e port))))

(define (display-nonprocedure-operator-exception e port)
  (print output: port "not a procedure. "
         (nonprocedure-operator-exception-operator e)))

(define (display-os-exception e port)
  (print output: port
         "OS exception "
         (os-exception-message e) #\,
         (os-exception-procedure e)
         (os-exception-arguments e)))

(define (display-no-such-file-or-directory-exception e port)
  (print output: port
         "no such file or directory. "
         (no-such-file-or-directory-exception-procedure e)
         (no-such-file-or-directory-exception-arguments e)))

(define (display-datum-parsing-exception e port)
  (print output: port
         "datum parsing exception. "
         (datum-parsing-exception-kind e) #\,
         (datum-parsing-exception-parameters e) #\,
         (datum-parsing-exception-readenv e)))

(define (display-expression-parsing-exception e port)
  (print output: port
         "expression parsing exception. "
         (expression-parsing-exception-kind e) #\,
         (expression-parsing-exception-parameters e) #\,
         (expression-parsing-exception-source e)))

(define (display-type-exception e port)
  (print output: port "type exception in "
         (type-exception-procedure e)
         (type-exception-arguments e)
         ". expected type for argument "
         (type-exception-arg-num e)
         " is "
         (type-exception-type-id e)))

(define (display-wrong-number-of-arguments-exception e port)
  (print output: port
         "wrong number of arguments. "
         (wrong-number-of-arguments-exception-procedure e)
         (wrong-number-of-arguments-exception-arguments e)))

(define (display-error-exception e port)
  (print output: port
         (error-exception-message e)
         " "
         (error-exception-parameters e)))

(define (display-keyword-expected-exception e port)
  (print output: port "keyword expected. "
         (keyword-expected-exception-procedure e)
         (keyword-expected-exception-arguments e)))

(define (display-number-of-arguments-limit-exception e port)
  (print output: port 
         "arguments limit reached. "
         (number-of-arguments-limit-exception-procedure e)
         (number-of-arguments-limit-exception-arguments e)))

(define (display-unknown-keyword-argument-exception e port)
  (print output: port 
         "unknown keyword. "
         (unknown-keyword-argument-exception-procedure e)
         (unknown-keyword-argument-exception-arguments e)))

(define (display-range-exception e port)
  (print output: port 
         "numeric parameter at position "
         (range-exception-arg-num e)
         " is not in the allowed range. "
         (range-exception-procedure e)
         (range-exception-arguments e)))

(define (display-divide-by-zero-exception e port)
  (print output: port 
         "division by zero."
         (divide-by-zero-exception-procedure e)
         (divide-by-zero-exception-arguments e)))

(define (display-improper-length-list-exception e port)
  (print output: port 
         "lists are not of the same length. "
         (improper-length-list-exception-procedure e)
         (improper-length-list-exception-arguments e)
         ", argument: "
         (improper-length-list-exception-arg-num e)))
