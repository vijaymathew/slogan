;; Copyright (c) 2013-2016 by Vijay Mathew Pandyalakal, All Rights Reserved.

(define-structure +macro+ params body)
(define make-macro make-+macro+)
(define macro-params +macro+-params)
(define macro-body +macro+-body)

(define *macros* (scm-list (make-table)))
(define *normal-vars* (scm-list (make-table)))

(define (def-macro name macro)
  (remove-normal-var-def name)
  (table-set! (scm-car *macros*) name macro))

(define (def-normal-var name)
  (table-set! (scm-car *normal-vars*) name #t))

(define (undef-macro name)
  (table-set! (scm-car *macros*) name #f))

(define (remove-macro-def name)
  (def-normal-var name)
  (if (get-macro-def name #f)
      (undef-macro name)))

(define (remove-normal-var-def name)
  (if (table-ref (scm-car *normal-vars*) name #f)
      (table-set! (scm-car *normal-vars*) name #f)))

(define (push-macros)
  (set! *macros* (scm-cons (make-table) *macros*)))

(define (pop-macros)
  (set! *macros* (scm-cdr *macros*)))

(define (push-normal-vars)
  (set! *normal-vars* (scm-cons (make-table) *normal-vars*)))

(define (pop-normal-vars)
  (set! *normal-vars* (scm-cdr *normal-vars*)))

(define (push-macros-lazy-fns)
  (push-macros)
  (push-normal-vars))

(define (pop-macros-lazy-fns)
  (pop-macros)
  (pop-normal-vars))

(define (remove-macro-lazy-fns-def name)
  (remove-macro-def name))

(define (get-macro-def name #!optional (drill #t)) (get-macro-lazy-def name *macros* drill))

(define (get-macro-lazy-def name tables drill)
  (let loop ((macros tables) (normal-vars *normal-vars*))
    (if (null? macros) #f
	(let ((m (table-ref (scm-car normal-vars) name #f)))
	  (if m #f
	      (let ((m (table-ref (scm-car macros) name #f)))
		(if m m
		    (if drill (loop (scm-cdr macros) (scm-cdr normal-vars)) #f))))))))

(define (push-func-params params)
  (if (list? params)
      (let loop ((params params))
	(if (scm-not (null? params))
	    (begin (cond ((valid-identifier? (scm-car params))
			  (remove-macro-lazy-fns-def (scm-car params)))
			 ((pair? (scm-car params))
			  (if (valid-identifier? (scm-caar params))
			      (remove-macro-lazy-fns-def (scm-caar params)))))
		   (loop (scm-cdr params)))))))

(define (sym->unquote sym)
  (let* ((s (symbol->string sym))
         (len (string-length s))
         (c (string-ref s 0)))
    (cond ((char=? c #\~)
           (if (> len 1)
               (if (char=? (string-ref s 1) #\%)
                   (scm-cons 'unquote-splicing (substring s 2 len))
                   (scm-cons 'unquote (substring s 1 len)))
               (scm-cons 'unquote (substring s 1 len))))
          (else #f))))

(define (escape-quotes-helper expr)
  (let loop ((expr expr) (escaped-expr '()))
    (if (null? expr)
        (scm-reverse escaped-expr)
        (let ((e (scm-car expr)))
          (if (symbol? e)
              (let ((uq (sym->unquote e)))
                (if uq
                    (loop (scm-cdr expr) (scm-cons (scm-list (scm-car uq) (string->symbol (scm-cdr uq)))
                                                   escaped-expr))
                    (loop (scm-cdr expr (scm-cons e escaped-expr)))))
              (loop (scm-cdr expr) (scm-cons s escaped-expr)))))))
              
(define (escape-quotes expr)
  (if (not (pair? expr))
      expr
      (let loop ((expr expr) (escaped-expr '()))
        (cond ((null? expr)
               (scm-reverse escaped-expr))
              (else
               (let ((e (scm-car expr)))
                 (display e) (newline)
                 (cond ((pair? e)
                        (loop (scm-cdr expr) (scm-cons (escape-quotes e) escaped-expr)))
                       ((eq? 'quasiquote e)
                        (loop (scm-cdr expr) (scm-cons (escape-quotes-helper e) escaped-expr)))
                       (else (loop (scm-cdr expr) (scm-cons e escaped-expr))))))))))
