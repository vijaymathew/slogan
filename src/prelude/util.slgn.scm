;; Copyright (c) 2013-2014 by Vijay Mathew Pandyalakal, All Rights Reserved.

(define (scm-repr->slgn-repr val)
  (if (or (number? val) 
          (string? val)
          (char? val)) 
      val
      (repr-convert val *scheme-reprs*)))

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
      str
      #\_ #\-))))

(define (slgn-symbol->scm-keyword s)
  (slgn-symbol->scm-sym/kw s string->keyword))

(define (slgn-symbol-quote? s)
  (and (list? s) (not (null? s))
       (eq? (car s) 'quote)))

(define (slgn-path/settings->scm-path/settings path-or-settings)
  (if (string? path-or-settings)
      path-or-settings
      (let loop ((settings path-or-settings)
                 (result '()))
        (if (null? settings)
            result
            (loop (cdr settings)
                  (append (list (slgn-symbol->scm-keyword (car (car settings)))
                                (cdr (car settings)))
                          result))))))

(define (repr-convert val reprs)
  (let ((r (assq val reprs)))
    (if r (cdr r) val)))

(define (void? val) (eq? *void* val))

(define (slgn-display val #!key display-string (port (current-output-port)))
  (if (not (void? val))
      (cond ((procedure? val)
             (slgn-display-function port))
            ((list? val)
             (slgn-display-list val port))
            ((pair? val)
             (slgn-display-pair val port))
            ((u8vector? val)
             (slgn-display-array val port "#u8" u8vector->list))
            ((s8vector? val)
             (slgn-display-array val port "#s8" s8vector->list))
            ((%bitvector? val)
             (slgn-display-array val port "#b" bitvector->list))
            ((vector? val)
             (slgn-display-array val port "#" vector->list))
            ((char? val)
             (slgn-display-char val port))
            ((string? val)
             (if display-string
                 (display val port)
                 (write val port)))
	    ((thread? val)
	     (slgn-display-task port))
            ((error-exception? val)
             (display-exception val port))
            ((eof-object? val)
             (display '<eof> port))
            ((reactive-var? val)
             (slgn-display-rvar port))
            ((##promise? val)
             (slgn-display-promise port))
            ((condition-variable? val)
             (slgn-display-special-obj "monitor" port))
            (else
             (display (scm-repr->slgn-repr val) port)))))

(define (slgn-display-special-obj tag port)
  (display "<" port) (display tag port)
  (display ">" port))
  
(define (slgn-display-rvar port) 
  (slgn-display-special-obj "rvar" port))

(define (slgn-display-function port)
  (slgn-display-special-obj "function" port))

(define (slgn-display-promise port)
  (slgn-display-special-obj "promise" port))

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

(define (slgn-display-array a port 
                            prefix tolist)
  (display prefix port)
  (slgn-display-list (tolist a) port))

(define (slgn-display-char c port)
  (display "'" port)
  (with-exception-catcher
   (lambda (ex)
     (let ((s (with-output-to-string
                '()
                (lambda () (write c)))))
       (if (char=? #\# (string-ref s 0))
           (display (substring s 1 (string-length s)) port)
           (display s port))))
   (lambda () (print port: port c)))
  (display "'" port))

(define (exception_to_string e)
  (call-with-output-string 
   '()
   (lambda (s)
     (display-exception e s))))

(define (slgn-display-task port)
  (slgn-display-special-obj "task" port))

(define (generic-map1! f result vec len reff setf)
  (let loop ((i 0))
    (if (>= i len) 
        (if result result *void*)
        (let ((res (f (reff vec i))))
          (if result (setf result i res))
          (loop (+ i 1))))))

(define (generic-map2+! f result vecs len reff setf)
  (let loop ((i 0))
    (if (>= i len) 
        (if result result *void*)
        (let ((res (apply f (reff vecs i)))) 
          (if result (setf result i res))
          (loop (+ i 1))))))

(define (assert-equal-lengths seq rest #!optional (lenf length))
  (let ((len (lenf seq)))
    (let loop ((rest rest))
      (if (not (null? rest))
          (begin (if (not (eq? (lenf (car rest)) len))
                     (error (with-output-to-string 
                              "Object is not of proper length: " 
                              (lambda () (slgn-display (car rest))))))
                 (loop (cdr rest)))))))
