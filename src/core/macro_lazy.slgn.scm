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

(define-structure macro-env bindings)

(define (print-env env)
    (let loop ((env (macro-env-bindings env)))
      (if (scm-not (null? env))
          (begin (newline)
                 (table-for-each (lambda (k v) 
                                   (display k)
                                   (display ": ")
                                   (display v)
                                   (newline))
                                   (scm-car env))
                 (loop (scm-cdr env))))))

(define (get-macro-env-value env param default-value)
  (let loop ((bindings (macro-env-bindings env)))
    (if (null? bindings) default-value
        (let ((v (table-ref (scm-car bindings) param default-value)))
          (if (scm-eq? v '*unbound*) 
              (loop (scm-cdr bindings)) 
              v)))))

(define (top-push-macro-env! env)
  (let ((new-t (make-table))
        (t (scm-car (macro-env-bindings env))))
    (table-for-each (lambda (k v) (table-set! new-t k v)) t)
    (macro-env-bindings-set! env (scm-cons new-t (macro-env-bindings env)))))

(define (update-macro-env! env sym)
  (let ((t (scm-car (macro-env-bindings env))))
    (if (scm-not (scm-eq? (table-ref t sym '*unbound*) '*unbound*))
        (table-set! t sym sym))
    env))

(define (push-macro-env! env expr extractor)
  (top-push-macro-env! env)
  (let ((t (scm-car (macro-env-bindings env))))
    (let loop ((expr expr))
      (if (null? expr) env
          (let ((sym (extractor expr)))
            (if (scm-not (scm-eq? (table-ref t sym '*unbound*) '*unbound*))
                (table-set! t sym sym))
            (loop (scm-cdr expr)))))))

(define (pop-macro-env! env)
  (if (null? (macro-env-bindings env))
      (error "Macro environment bindings cannot be null!")
      (macro-env-bindings-set! env (scm-cdr (macro-env-bindings env))))
  env)
      
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

