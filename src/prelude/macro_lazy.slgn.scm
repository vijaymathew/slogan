;; Copyright (c) 2013-2014 by Vijay Mathew Pandyalakal, All Rights Reserved.

(define-structure +macro+ params body)
(define make-macro make-+macro+)
(define make-lazy make-+macro+)
(define macro-params +macro+-params)
(define macro-body +macro+-body)

(define *macros* (list (make-table)))
(define *lazy-fns* (list (make-table)))
(define *normal-vars* (list (make-table)))

(define (def-macro name macro)
  (remove-normal-var-def name)
  (table-set! (car *macros*) name macro))

(define (def-lazy name lazy)
  (remove-normal-var-def name)
  (table-set! (car *lazy-fns*) name lazy))

(define (def-normal-var name)
  (table-set! (car *normal-vars*) name #t))

(define (undef-macro name)
  (table-set! (car *macros*) name #f))

(define (undef-lazy name)
  (table-set! (car *lazy-fns*) name #f))

(define (remove-macro-def name)
  (def-normal-var name)
  (if (get-macro-def name #f)
      (undef-macro name)))

(define (remove-lazy-def name)
  (def-normal-var name)
  (if (get-lazy-def name #f)
      (undef-lazy name)))

(define (remove-normal-var-def name)
  (if (table-ref (car *normal-vars*) name #f)
      (table-set! (car *normal-vars*) name #f)))

(define (push-macros)
  (set! *macros* (cons (make-table) *macros*)))

(define (push-lazy-fns)
  (set! *lazy-fns* (cons (make-table) *lazy-fns*)))

(define (pop-macros)
  (set! *macros* (cdr *macros*)))

(define (pop-lazy-fns)
  (set! *lazy-fns* (cdr *lazy-fns*)))

(define (push-normal-vars)
  (set! *normal-vars* (cons (make-table) *normal-vars*)))

(define (pop-normal-vars)
  (set! *normal-vars* (cdr *normal-vars*)))

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
	(let ((m (table-ref (car normal-vars) name #f)))
	  (if m #f
	      (let ((m (table-ref (car macros) name #f)))
		(if m m
		    (if drill (loop (cdr macros) (cdr normal-vars)) #f))))))))

(define-structure macro-env bindings)

(define (get-macro-env-value env param default-value)
  (table-ref (car (macro-env-bindings env)) param default-value))

(define (top-push-macro-env! env)
  (let ((new-t (make-table))
        (t (car (macro-env-bindings env))))
    (table-for-each (lambda (k v) (table-set! new-t k v)) t)
    (macro-env-bindings-set! env (cons new-t (macro-env-bindings env)))))

(define (update-macro-env! env sym)
  (let ((t (car (macro-env-bindings env))))
    (if (not (eq? (table-ref t sym '*unbound*) '*unbound*))
        (table-set! t sym sym))
    env))

(define (push-macro-env! env expr extractor)
  (top-push-macro-env! env)
  (let ((t (car (macro-env-bindings env))))
    (let loop ((expr expr))
      (if (null? expr) 
          env
          (let ((sym (extractor expr)))
            (if (not (eq? (table-ref t sym '*unbound*) '*unbound*))
                (table-set! t sym sym))
            (loop (cdr expr)))))))

(define (pop-macro-env! env)
  (if (null? (macro-env-bindings env))
      (error "Macro environment bindings cannot be null!")
      (macro-env-bindings-set! env (cdr (macro-env-bindings env))))
  env)

(define (params-args->table params args)
  (let ((t (make-table)))
    (let loop ((params params)
               (args args))
      (if (null? params)
          t
          (begin (table-set! t (car params) (car args))
                 (loop (cdr params) (cdr args)))))))

(define (replace-macro-args params args body)
  (replace-macro-args-helper 
   body
   (make-macro-env (list (params-args->table params args)))))

(define (replace-macro-var params args body)
  (if (eq? body (car params))
      (car args)
      body))

(define (replace-macro-args-helper expr env)
  (cond ((null? expr) expr)
        ((not (pair? expr))
         (if (symbol? expr)
             (get-macro-env-value env expr expr)
             expr))
        (else (let ((sym (car expr)))
                (cond ((or (eq? sym 'let) (eq? sym 'letrec) (eq? sym 'let*))
                       (let ((r (append (list sym (let ((vals (replace-let-vals (cadr expr) env)))
                                                    (if (null? vals) vals
                                                        (list vals))))
                                        (replace-macro-args-helper 
                                         (cddr expr) 
                                         (push-macro-env! env (cadr expr) caar)))))
                         (pop-macro-env! env)
                         r))
                      ((eq? sym 'lambda)
                       (let ((r (list sym (cadr expr)
                                      (replace-macro-args-helper
                                       (cddr expr)
                                       (push-macro-env! env (cadr expr) car)))))
                         (pop-macro-env! env)
                         r))
                      ((eq? sym 'define)
                       (let ((r (append (list sym (cadr expr)) 
                                        (replace-macro-args-helper
                                         (cddr expr)
                                         env))))
                         (update-macro-env! env (cadr expr))
                         r))
                      (else (let ((a (replace-macro-args-helper sym env))
                                  (b (replace-macro-args-helper (cdr expr) env)))
                              (cons a b))))))))

(define (replace-let-vals expr env)
  (let loop ((expr expr)
             (result '()))
    (if (null? expr)
        result
        (let ((v (replace-macro-args-helper (cadar expr) env)))
          (loop (cdr expr) 
                (append result (cons (caar expr) (list v))))))))

(define (expr-lazify lazy-fn expr)
  (if lazy-fn
      (list 'delay expr)
      expr))

(define (normalize-lazy-params params)
  (let loop ((params params)
	     (result '()))
    (cond ((null? params)
	   (reverse result))
	  ((or (eq? #!optional (car params))
	       (eq? #!key (car params))
	       (eq? #!rest (car params)))
	   (loop (cdr params) result))
	  ((list? (car params))
	   (loop (cdr params) (cons (caar params) result)))
	  (else (loop (cdr params) (cons (car params) result))))))

(define (expr-forcify expr params)
  (let ((params (normalize-lazy-params params)))
    (replace-macro-args params (map (lambda (x) (list 'force x)) params) expr)))
