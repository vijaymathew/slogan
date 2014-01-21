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
    ((!optional) '#!optional)
    ((!key) '#!key)
    ((!rest) '#!rest)
    (else s)))

(define (slogan-variable->scheme-keyword var)
  (string->keyword (symbol->string var)))

(define (slogan-symbol? s)
  (and (symbol? s)
       (char=? (string-ref (symbol->string s) 0) #\!)))

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
  (display "(")
  (display (car p))
  (display " ")
  (display (cdr p))
  (display ")"))

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
         (print "unbound global " (unbound-global-exception-variable e)))
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
  (print "type exception in " (type-exception-procedure e)
         (type-exception-arguments e)
         ". expected type for argument "
         (type-exception-arg-num e) " is "
         (type-exception-type-id e)))

(define (display-wrong-number-of-arguments-exception e)
  (print "wrong number of arguments. "
         (wrong-number-of-arguments-exception-procedure e)
         (wrong-number-of-arguments-exception-arguments e)))

(define (display-error-exception e)
  (print (error-exception-message e)
         (error-exception-parameters e)))

(define (display-keyword-expected-exception e)
  (print "keyword expected. "
         (keyword-expected-exception-procedure e)
         (keyword-expected-exception-arguments e)))

(define (display-number-of-arguments-limit-exception e)
  (print "arguments limit reached. "
         (number-of-arguments-limit-exception-procedure e)
         (number-of-arguments-limit-exception-arguments e)))

(define (display-unknown-keyword-argument-exception e)
  (print "unknown keyword."
         (unknown-keyword-argument-exception-procedure e)
         (unknown-keyword-argument-exception-arguments e)))
