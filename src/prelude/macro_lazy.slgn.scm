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
  (undef-lazy name)
  (table-set! (car *macros*) name macro))

(define (def-lazy name lazy)
  (remove-normal-var-def name)
  (undef-macro name)
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

(define (print-env env)
    (let loop ((env (macro-env-bindings env)))
      (if (not (null? env))
          (begin (newline)
                 (table-for-each (lambda (k v) 
                                   (display k)
                                   (display ": ")
                                   (display v)
                                   (newline))
                                   (car env))
                 (loop (cdr env))))))

(define (get-macro-env-value env param default-value)
  (let loop ((bindings (macro-env-bindings env)))
    (if (null? bindings) default-value
        (let ((v (table-ref (car bindings) param default-value)))
          (if (eq? v '*unbound*) 
              (loop (cdr bindings)) 
              v)))))

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
      (if (null? expr) env
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
      (if (null? params) t
          (begin (table-set! t (car params) (car args))
                 (loop (cdr params) (cdr args)))))))

(define (replace-macro-args params args body is-lazyfn)
  (replace-macro-args-helper 
   body
   (make-macro-env (list (params-args->table params args))) is-lazyfn))

(define (replace-macro-var params args body)
  (let loop ((params params)
             (args args))
    (cond ((null? params) body)
          ((eq? body (car params)) (car args))
          (else (loop (cdr params) (cdr args))))))

(define (replace-macro-args-helper expr env is-lazyfn)
  (cond ((null? expr) expr)
        ((not (pair? expr))
         (if (symbol? expr)
             (get-macro-env-value env expr expr)
             expr))
        (else (let ((sym (car expr)))
                (cond ((or (eq? sym 'let) (eq? sym 'letrec) (eq? sym 'let*))
                       (let* ((named-let (symbol? (cadr expr)))
                              (let-expr (if named-let (list sym (cadr expr))
                                            (list sym))))
                         (let ((vals (let ((v (replace-let-vals 
                                               (if named-let (caddr expr) (cadr expr))
                                               env is-lazyfn)))
                                       (if (null? v) v (list v)))))
                           (let ((r (append let-expr (if (null? vals) (list vals) vals)
                                            (replace-macro-args-helper 
                                             (if named-let (cdddr expr) (cddr expr))
                                             (if (null? vals) env (push-macro-env! env (car vals) caar)) 
                                             is-lazyfn))))
                             (if (not (null? vals)) (pop-macro-env! env))
                             r))))
                      ((eq? sym 'lambda)
                       (let ((r (append (list sym (if is-lazyfn (cadr expr) 
                                                      (replace-macro-args-helper (cadr expr) env is-lazyfn)))
                                        (replace-macro-args-helper
                                         (cddr expr)
                                         (push-macro-env! env (cadr expr) car) is-lazyfn))))
                         (pop-macro-env! env)
                         r))
                      ((eq? sym 'define)
                       (let ((r (append (list sym (if is-lazyfn (cadr expr) 
                                                      (replace-macro-args-helper (cadr expr) env is-lazyfn)))
                                        (replace-macro-args-helper
                                         (cddr expr)
                                         env is-lazyfn))))
                         (update-macro-env! env (cadr expr))
                         r))
                      ((eq? sym 'set!)
                       (append (list sym (if is-lazyfn (cadr expr) 
                                             (replace-macro-args-helper (cadr expr) env is-lazyfn)))
                               (replace-macro-args-helper (cddr expr) env is-lazyfn)))
                      ((eq? sym '@eval)
                       (if is-lazyfn (error "@eval can be used only in a macro."))
                       (eval (car (replace-macro-args-helper (cdr expr) env is-lazyfn))))
                      (else (let ((a (replace-macro-args-helper sym env is-lazyfn))
                                  (b (replace-macro-args-helper (cdr expr) env is-lazyfn)))
                              (cons a b))))))))

(define (replace-let-vals expr env is-lazyfn)
  (let loop ((expr expr)
             (result '()))
    (if (null? expr)
        result
        (let ((v (replace-macro-args-helper (cadar expr) env is-lazyfn)))
          (loop (cdr expr) 
                (append result (list (cons (if is-lazyfn (caar expr) 
                                               (replace-macro-args-helper (caar expr) env is-lazyfn)) 
                                           (list v)))))))))

(define (expr-lazify lazy-fn expr)
  (if lazy-fn
      (list 'delay expr)
      expr))

(define (normalize-lazy-params params)
  (if (symbol? params) (list params)
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
              (else (loop (cdr params) (cons (car params) result)))))))

(define (expr-forcify expr params)
  (let ((params (normalize-lazy-params params)))
    (replace-macro-args params (map (lambda (x) (list 'force x)) params) expr #t)))

(define (declare-lazy name)
  (if (symbol? name)
      (if (not (get-lazy-def name))
          (def-lazy name (make-lazy #f #f)))
      (let loop ((names name))
        (if (not (null? names))
            (begin (declare-lazy (car names))
                   (loop (cdr names)))))))
      
