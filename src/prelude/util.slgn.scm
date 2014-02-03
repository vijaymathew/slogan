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

(define (slogan-display val)
  (cond ((void? val)
         #f)
        ((procedure? val)
         (display "[function]"))
        ((list? val)
         (slogan-display-list val))
        ((pair? val)
         (slogan-display-pair val))
        ((vector? val)
         (slogan-display-array val))
	((char? val)
	 (slogan-display-char val))
        ((string? val)
         (write val))
        (else
         (display (scheme-repr->slogan-repr val))
         #t)))

(define (slogan-display-list lst)
  (display "[")
  (let loop ((lst lst))
    (cond ((null? lst)
           (display "]")
           #t)
          (else
           (slogan-display (car lst))
           (if (not (null? (cdr lst)))
               (display ", "))
           (loop (cdr lst))))))

(define (slogan-display-pair p)
  (display "[")
  (slogan-display (car p))
  (display " ")
  (slogan-display (cdr p))
  (display "]"))

(define (slogan-display-array a)
  (display "#")
  (slogan-display-list (vector->list a)))
  
(define (slogan-display-char c)
  (display "'")
  (display c)
  (display "'"))

(define (print . args)
  (let loop ((args args))
    (if (not (null? args))
        (begin (slogan-display (car args))
               (loop (cdr args))))))

(define (println . args)
  (let loop ((args args))
    (if (not (null? args))
        (begin (slogan-display (car args))
               (loop (cdr args)))
        (newline))))

(define (display-exception e)
  (display "error: ")
  (cond ((error-exception? e)
         (display-error-exception e))
        ((unbound-global-exception? e)
         (display "unbound global ") 
         (display (unbound-global-exception-variable e)))
        ((type-exception? e)
         (display-type-exception e))
        ((nonprocedure-operator-exception? e)
         (display-nonprocedure-operator-exception e))
        ((wrong-number-of-arguments-exception? e)
         (display-wrong-number-of-arguments-exception e))
        ((keyword-expected-exception? e)
         (display-keyword-expected-exception e))
        ((number-of-arguments-limit-exception? e)
         (display-number-of-arguments-limit-exception e))
        ((unknown-keyword-argument-exception? e) 
         (display-unknown-keyword-argument-exception e))
        ((datum-parsing-exception? e)
         (display-datum-parsing-exception e))
        ((expression-parsing-exception? e)
         (display-expression-parsing-exception e))
        ((os-exception? e)
         (display-os-exception e))
        ((no-such-file-or-directory-exception? e)
         (display-no-such-file-or-directory-exception e))
        ((heap-overflow-exception? e)
         (display "heap overflow."))
        ((stack-overflow-exception? e)
         (display "stack overflow."))
        ((range-exception? e)
         (display-range-exception e))
        ((divide-by-zero-exception? e)
         (display-divide-by-zero-exception e))
        ((improper-length-list-exception? e)
         (display-improper-length-list-exception e))
        (else (display e)))
  (newline))

(define (display-nonprocedure-operator-exception e)
  (display "not a procedure. ")
  (display (nonprocedure-operator-exception-operator e)))

(define (display-os-exception e)
  (print "OS exception " (os-exception-message e) ", "
         (os-exception-procedure e)
         (os-exception-arguments e)))

(define (display-no-such-file-or-directory-exception e)
  (print "no such file or directory. "
         (no-such-file-or-directory-exception-procedure e)
         (no-such-file-or-directory-exception-arguments e)))

(define (display-datum-parsing-exception e)
  (print "datum parsing exception. "
         (datum-parsing-exception-kind e) ","
         (datum-parsing-exception-parameters e) ", "
         (datum-parsing-exception-readenv e)))

(define (display-expression-parsing-exception e)
  (print "expression parsing exception. "
         (expression-parsing-exception-kind e) ", "
         (expression-parsing-exception-parameters e) ", "
         (expression-parsing-exception-source e)))

(define (display-type-exception e)
  (display "type exception in ")
  (display (type-exception-procedure e))
  (display (type-exception-arguments e))
  (display ". expected type for argument ")
  (display (type-exception-arg-num e))
  (display " is ")
  (display (type-exception-type-id e)))

(define (display-wrong-number-of-arguments-exception e)
  (display "wrong number of arguments. ")
  (display (wrong-number-of-arguments-exception-procedure e))
  (display (wrong-number-of-arguments-exception-arguments e)))

(define (display-error-exception e)
  (display (error-exception-message e))
  (display " ")
  (display (error-exception-parameters e)))

(define (display-keyword-expected-exception e)
  (display "keyword expected. ")
  (display (keyword-expected-exception-procedure e))
  (display (keyword-expected-exception-arguments e)))

(define (display-number-of-arguments-limit-exception e)
  (display "arguments limit reached. ")
  (display (number-of-arguments-limit-exception-procedure e))
  (display (number-of-arguments-limit-exception-arguments e)))

(define (display-unknown-keyword-argument-exception e)
  (display "unknown keyword. ")
  (display (unknown-keyword-argument-exception-procedure e))
  (display (unknown-keyword-argument-exception-arguments e)))

(define (display-range-exception e)
  (display "numeric parameter at position ")
  (display (range-exception-arg-num e))
  (display " is not in the allowed range. ")
  (display (range-exception-procedure e))
  (display (range-exception-arguments e)))

(define (display-divide-by-zero-exception e)
  (display "division by zero.")
  (display (divide-by-zero-exception-procedure e))
  (display (divide-by-zero-exception-arguments e)))

(define (display-improper-length-list-exception e)
  (display "lists are not of the same length. ")
  (display (improper-length-list-exception-procedure e))
  (display (improper-length-list-exception-arguments e))
  (display ", argument: ")
  (display (improper-length-list-exception-arg-num e)))
