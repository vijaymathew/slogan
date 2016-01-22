;; Copyright (c) 2013-2016 by Vijay Mathew Pandyalakal, All Rights Reserved.

(define-structure +macro+ params body)
(define make-macro make-+macro+)
(define make-lazy make-+macro+)
(define macro-params +macro+-params)
(define macro-body +macro+-body)

(define *macros* (scm-list (make-table)))
(define *lazy-fns* (scm-list (make-table)))
(define *normal-vars* (scm-list (make-table)))

(define (def-macro name macro)
  (remove-normal-var-def name)
  (undef-lazy name)
  (table-set! (scm-car *macros*) name macro))

(define (def-lazy name lazy)
  (remove-normal-var-def name)
  (undef-macro name)
  (table-set! (scm-car *lazy-fns*) name lazy))

(define (def-normal-var name)
  (table-set! (scm-car *normal-vars*) name #t))

(define (undef-macro name)
  (table-set! (scm-car *macros*) name #f))

(define (undef-lazy name)
  (table-set! (scm-car *lazy-fns*) name #f))

(define (remove-macro-def name)
  (def-normal-var name)
  (if (get-macro-def name #f)
      (undef-macro name)))

(define (remove-lazy-def name)
  (def-normal-var name)
  (if (get-lazy-def name #f)
      (undef-lazy name)))

(define (remove-normal-var-def name)
  (if (table-ref (scm-car *normal-vars*) name #f)
      (table-set! (scm-car *normal-vars*) name #f)))

(define (push-macros)
  (set! *macros* (scm-cons (make-table) *macros*)))

(define (push-lazy-fns)
  (set! *lazy-fns* (scm-cons (make-table) *lazy-fns*)))

(define (pop-macros)
  (set! *macros* (scm-cdr *macros*)))

(define (pop-lazy-fns)
  (set! *lazy-fns* (scm-cdr *lazy-fns*)))

(define (push-normal-vars)
  (set! *normal-vars* (scm-cons (make-table) *normal-vars*)))

(define (pop-normal-vars)
  (set! *normal-vars* (scm-cdr *normal-vars*)))

(define (push-macros-lazy-fns)
  (push-macros)
  (push-lazy-fns)
  (push-normal-vars))

(define (pop-macros-lazy-fns)
  (pop-macros)
  (pop-lazy-fns)
  (pop-normal-vars))

(define (remove-macro-lazy-fns-def name)
  (remove-macro-def name)
  (remove-lazy-def name))

(define (get-macro-def name #!optional (drill #t)) (get-macro-lazy-def name *macros* drill))
(define (get-lazy-def name #!optional (drill #t)) (get-macro-lazy-def name *lazy-fns* drill))

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

(define (params-args->table params args)
  (let ((t (make-table)))
    (let loop ((params params)
               (args args))
      (if (null? params) t
          (begin (table-set! t (scm-car params) (scm-car args))
                 (loop (scm-cdr params) (scm-cdr args)))))))

(define (replace-macro-args params args body)
  (replace-macro-args-helper 
   body
   (make-macro-env (scm-list (params-args->table params args)))))

(define (replace-macro-var params args body)
  (let loop ((params params)
             (args args))
    (cond ((null? params) body)
          ((scm-eq? body (scm-car params)) (scm-car args))
          (else (loop (scm-cdr params) (scm-cdr args))))))

(define (replace-macro-args-helper expr env)
  (cond ((null? expr) expr)
        ((scm-not (pair? expr))
         (if (symbol? expr)
             (get-macro-env-value env expr expr)
             expr))
        (else (let ((sym (scm-car expr)))
                (cond ((or (scm-eq? sym 'let) (scm-eq? sym 'letrec) (scm-eq? sym 'let*))
                       (let* ((named-let (symbol? (scm-cadr expr)))
                              (let-expr (if named-let (scm-list sym (scm-cadr expr))
                                            (scm-list sym))))
                         (let ((vals (let ((v (replace-let-vals 
                                               (if named-let (scm-caddr expr) (scm-cadr expr))
                                               env)))
                                       (if (null? v) v (scm-list v)))))
                           (let ((r (scm-append let-expr (if (null? vals) (scm-list vals) vals)
                                            (replace-macro-args-helper 
                                             (if named-let (cdddr expr) (cddr expr))
                                             (if (null? vals) env (push-macro-env! env (scm-car vals) caar))))))
                             (if (scm-not (null? vals)) (pop-macro-env! env))
                             r))))
                      ((scm-eq? sym 'lambda)
                       (let ((r (scm-append (scm-list sym (scm-cadr expr))
                                        (replace-macro-args-helper
                                         (cddr expr)
                                         (push-macro-env! env (scm-cadr expr) car)))))
                         (pop-macro-env! env)
                         r))
                      ((scm-eq? sym 'define)
                       (let ((r (scm-append (scm-list sym (scm-cadr expr))
                                        (replace-macro-args-helper
                                         (cddr expr)
                                         env))))
                         (update-macro-env! env (scm-cadr expr))
                         r))
                      ((scm-eq? sym 'set!)
                       (scm-append (scm-list sym (scm-cadr expr))
                               (replace-macro-args-helper (cddr expr) env)))
                      (else (let ((a (replace-macro-args-helper sym env))
                                  (b (replace-macro-args-helper (scm-cdr expr) env)))
                              (scm-cons a b))))))))

(define (replace-let-vals expr env)
  (let loop ((expr expr)
             (result '()))
    (if (null? expr)
        result
        (let ((v (replace-macro-args-helper (scm-cadar expr) env)))
          (loop (scm-cdr expr) 
                (scm-append result (scm-list (scm-cons (scm-caar expr)
                                           (scm-list v)))))))))

(define (expr-lazify lazy-fn expr)
  (if lazy-fn
      (scm-list 'delay expr)
      expr))

(define (normalize-lazy-params params)
  (if (symbol? params) (scm-list params)
      (let loop ((params params)
                 (result '()))
        (cond ((null? params)
               (scm-reverse result))
              ((or (scm-eq? #!optional (scm-car params))
                   (scm-eq? #!key (scm-car params))
                   (scm-eq? #!rest (scm-car params)))
               (loop (scm-cdr params) result))
              ((list? (scm-car params))
               (loop (scm-cdr params) (scm-cons (scm-caar params) result)))
              (else (loop (scm-cdr params) (scm-cons (scm-car params) result)))))))

(define (expr-forcify expr params)
  (let ((params (normalize-lazy-params params)))
    (replace-macro-args params (scm-map (lambda (x) (scm-list 'scm-force x)) params) expr)))

(define (declare-lazy name)
  (if (symbol? name)
      (if (scm-not (get-lazy-def name))
          (def-lazy name (make-lazy #f #f)))
      (let loop ((names name))
        (if (scm-not (null? names))
            (begin (declare-lazy (scm-car names))
                   (loop (scm-cdr names)))))))
      
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

