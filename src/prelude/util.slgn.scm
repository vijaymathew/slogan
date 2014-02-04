;; Copyright (c) 2013-2014 by Vijay Mathew Pandyalakal, All Rights Reserved.

(define *slogan-reprs* '((true . #t) (false . #f)))                         
(define *scheme-reprs* '((#t . true) (#f . false)))

(define (scheme-repr->slogan-repr val)
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

(define (slogan-repr->scheme-repr val)
  (repr-convert val *slogan-reprs*))

(define (slogan-directive->scheme-directive s)
  (case s
    ((@optional) '#!optional)
    ((@key) '#!key)
    ((@rest) '#!rest)
    (else s)))

(define (slogan-variable->scheme-keyword var)
  (string->keyword (symbol->string var)))

(define (slogan-symbol->scheme-sym/kw s convfn)
  (let ((str (symbol->string s)))
    (convfn
     (string_replace_all 
      (substring
       str
       1 (string-length str))
      #\_ #\-))))

(define (slogan-symbol->scheme-keyword s)
  (slogan-symbol->scheme-sym/kw s string->keyword))

(define (slogan-symbol->scheme-symbol s)
  (slogan-symbol->scheme-sym/kw s string->symbol))

(define (slogan-symbol? s)
  (and (symbol? s)
       (char=? (string-ref (symbol->string s) 0) #\!)))

(define (scm-symbol->slgn-symbol s)
  (let ((str (symbol->string s)))
    (string->symbol (substring str 1 (string-length str)))))

(define (repr-convert val reprs)
  (let ((r (assq val reprs)))
    (if r (cdr r) val)))

(define (void? val) (eq? '#!void val))

(define (slogan-display val #!key display-string (port (current-output-port)))
  (cond ((void? val)
         #f)
        ((procedure? val)
         (slogan-display-function val port))
        ((list? val)
         (slogan-display-list val port))
        ((pair? val)
         (slogan-display-pair val port))
        ((vector? val)
         (slogan-display-array val port))
	((char? val)
	 (slogan-display-char val port))
        ((string? val)
         (if display-string
             (display val port)
             (write val port)))
        ((error-exception? val)
         (display-exception val port))
        (else
         (display (scheme-repr->slogan-repr val) port)
         #t)))

(define (slogan-display-function proc port)
  (let ((name (with-output-to-string '() 
                                     (lambda () (display proc)))))
    (display "[function " port)
    (let loop ((parts (cddr (string_split name '(#\space #\< #\>)))))
      (if (not (null? parts))
          (begin (display (car parts) port)
                 (if (pair? (cdr parts)) (display #\space port))
                 (loop (cdr parts)))
          (display "]" port)))))
      
(define (slogan-display-list lst port)
  (display "[" port)
  (let loop ((lst lst))
    (cond ((null? lst)
           (display "]" port)
           #t)
          (else
           (slogan-display (car lst) port: port)
           (if (not (null? (cdr lst)))
               (display ", " port))
           (loop (cdr lst))))))

(define (slogan-display-pair p port)
  (display "[" port)
  (slogan-display (car p) port: port)
  (display " " port)
  (slogan-display (cdr p) port: port)
  (display "]" port))

(define (slogan-display-array a port)
  (display "#" port)
  (slogan-display-list (vector->list a) port))
  
(define (slogan-display-char c port)
  (display "'" port)
  (scheme-print c port: port)
  (display "'") port)

(define scheme-print print)

(define (print #!key (stream (current-output-port)) #!rest args)
  (let loop ((args args))
    (if (not (null? args))
        (begin (slogan-display (car args) display-string: #t port: stream)
               (loop (cdr args))))))

(define scheme-println println)

(define (println #!key (stream (current-output-port)) #!rest args)
  (let loop ((args args))
    (if (not (null? args))
        (begin (slogan-display (car args) display-string: #t port: stream)
               (loop (cdr args)))
        (newline stream))))

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
  (print stream: port "not a procedure. "
         (nonprocedure-operator-exception-operator e)))

(define (display-os-exception e port)
  (print stream: port
         "OS exception "
         (os-exception-message e) #\,
         (os-exception-procedure e)
         (os-exception-arguments e)))

(define (display-no-such-file-or-directory-exception e port)
  (print stream: port
         "no such file or directory. "
         (no-such-file-or-directory-exception-procedure e)
         (no-such-file-or-directory-exception-arguments e)))

(define (display-datum-parsing-exception e port)
  (print stream: port
         "datum parsing exception. "
         (datum-parsing-exception-kind e) #\,
         (datum-parsing-exception-parameters e) #\,
         (datum-parsing-exception-readenv e)))

(define (display-expression-parsing-exception e port)
  (print stream: port
         "expression parsing exception. "
         (expression-parsing-exception-kind e) #\,
         (expression-parsing-exception-parameters e) #\,
         (expression-parsing-exception-source e)))

(define (display-type-exception e port)
  (print stream: port "type exception in "
         (type-exception-procedure e)
         (type-exception-arguments e)
         ". expected type for argument "
         (type-exception-arg-num e)
         " is "
         (type-exception-type-id e)))

(define (display-wrong-number-of-arguments-exception e port)
  (print stream: port
         "wrong number of arguments. "
         (wrong-number-of-arguments-exception-procedure e)
         (wrong-number-of-arguments-exception-arguments e)))

(define (display-error-exception e port)
  (print stream: port
         (error-exception-message e)
         " "
         (error-exception-parameters e)))

(define (display-keyword-expected-exception e port)
  (print stream: port "keyword expected. "
         (keyword-expected-exception-procedure e)
         (keyword-expected-exception-arguments e)))

(define (display-number-of-arguments-limit-exception e port)
  (print stream: port 
         "arguments limit reached. "
         (number-of-arguments-limit-exception-procedure e)
         (number-of-arguments-limit-exception-arguments e)))

(define (display-unknown-keyword-argument-exception e port)
  (print stream: port 
         "unknown keyword. "
         (unknown-keyword-argument-exception-procedure e)
         (unknown-keyword-argument-exception-arguments e)))

(define (display-range-exception e port)
  (print stream: port 
         "numeric parameter at position "
         (range-exception-arg-num e)
         " is not in the allowed range. "
         (range-exception-procedure e)
         (range-exception-arguments e)))

(define (display-divide-by-zero-exception e port)
  (print stream: port 
         "division by zero."
         (divide-by-zero-exception-procedure e)
         (divide-by-zero-exception-arguments e)))

(define (display-improper-length-list-exception e port)
  (print stream: port 
         "lists are not of the same length. "
         (improper-length-list-exception-procedure e)
         (improper-length-list-exception-arguments e)
         ", argument: "
         (improper-length-list-exception-arg-num e)))
