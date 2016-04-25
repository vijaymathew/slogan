;; Copyright (c) 2013-2016 by Vijay Mathew Pandyalakal, All Rights Reserved.

(define *namespaces* '())
(define *active-namespaces* (make-table))
(define *declared-imported* '())
(define *namespace-counter* -1)

(define (namespace_names namespace-name)
  (table-ref *active-namespaces* namespace-name #f))

(define (namespace-hide-unhide namespace-name symbols-or-symbol hide f)
  (let ((namespace (table-ref *active-namespaces* namespace-name #f)))
    (if namespace
        (let loop ((symbols (if (list? symbols-or-symbol) symbols-or-symbol (scm-list symbols-or-symbol)))
                   (new-names (if hide '() namespace)))
          (if (null? symbols)
              (let ((namespace (scm-reverse new-names)))
                (table-set! *active-namespaces* namespace-name namespace)
                namespace)
              (loop (scm-cdr symbols) (f namespace symbols new-names))))
        #f)))

(define (namespace_hide namespace-name symbols-or-symbol)
  (namespace-hide-unhide namespace-name symbols-or-symbol #t
                         (lambda (namespace symbols new-names)
                           (if (scm-eq? (scm-car namespace) (scm-car symbols))
                               new-names
                               (scm-cons (scm-car namespace) new-names)))))

(define (namespace_unhide namespace-name symbols-or-symbol)
  (namespace-hide-unhide namespace-name symbols-or-symbol #f
                         (lambda (namespace symbols new-names)
                           (if (scm-memq (scm-car symbols) namespace)
                               new-names
                               (scm-cons (scm-car symbols) new-names)))))

(define (namespace_delete namespace-name)
  (table-set! *active-namespaces* namespace-name #f)
  #t)

(define (namespace_create namespace-name symbols)
  (if (scm-not (symbol? namespace-name))
      (error "namespace name must be a symbol - " namespace-name)
      (if (and (list? symbols) (for_all symbol? symbols))
          (begin (table-set! *active-namespaces* namespace-name symbols)
                 #t)
          (error "exported names must be a list of symbols - " symbols))))

(define (engage-namespace-counter) (set! *namespace-counter* 0))

(define (incr-namespace-counter)
  (if (>= *namespace-counter* 0)
      (set! *namespace-counter* (+ *namespace-counter* 1))))

(define (decr-namespace-counter)
  (if (> *namespace-counter* 0)
      (set! *namespace-counter* (- *namespace-counter* 1))))

(define (disengage-namespace-counter)
  (if (> *namespace-counter* 0)
      (begin (pop-namespace)
	     (disengage-namespace-counter))
      (set! *namespace-counter* -1)))

(define (push-namespace name)
  (incr-namespace-counter)
  (set! *namespaces* (scm-cons (scm-cons (add-parent-namespace-names name) 
                                 (existing-namespace-defs name)) 
                           *namespaces*)))

(define (pop-namespace)
  (decr-namespace-counter)
  (if (scm-not (null? *namespaces*))
      (let ((top (scm-car *namespaces*)))
          (set! *namespaces* (scm-cdr *namespaces*))
          (let ((expr `(table-set! *active-namespaces* ',(scm-car top) ',(scm-cdr top))))
            (scm-eval expr)
            expr))
      #f))

(define (existing-namespace-defs name)
  (table-ref *active-namespaces* name '()))

(define (add-parent-namespace-names name)
  (if (null? *namespaces*) name
      (let loop ((namespaces (scm-reverse *namespaces*))
                 (sname ""))
        (if (null? namespaces)
            (string->symbol (string-append sname (symbol->string name)))
            (loop (scm-cdr namespaces)
                  (string-append sname (symbol->string (scm-caar namespaces)) "_"))))))

(define (import-from-namespace name #!optional defs prefix)
  (let ((n (find-namespace name)))
    (if (and (scm-not n) (scm-not defs)) (error "namespace not found -" name))
    (if (and n defs) (validate-import-names defs n))
    (let loop ((defs (if defs defs n))
               (imports '()))
      (if (null? defs)
          (scm-cons 'begin imports)
          (let ((fdef (scm-car defs)))
            (let ((import-name (get-import-name fdef prefix))
                  (n (get-name-from-namespace fdef name)))
              (loop (scm-cdr defs)
                    (let ((def-expr `(,(if (in-declared-imports? import-name) 'set! 'define)
                                      ,import-name ,n)))
                      (let ((compiler-defs-expr `(cond ((get-macro-def ',n)
                                                        (def-macro ',import-name #t))
                                                       (else
                                                        (let ((d (get-lazy-def ',n)))
                                                          (if d
                                                              (def-lazy ',import-name d)))))))
                        (if (get-macro-def n)
                            (scm-eval def-expr))
                        (scm-eval compiler-defs-expr)
                        (append `(,def-expr ,compiler-defs-expr) imports))))))))))

(define (validate-import-names defs namespace)
  (let loop ((defs defs))
    (if (null? defs) #t
        (if (scm-not (scm-memq (scm-car defs) namespace))
            (error "Name not found in namespace." (scm-car defs))
            (loop (scm-cdr defs))))))

(define (find-namespace name)
  (table-ref *active-namespaces* name #f))

(define (get-name-from-namespace def namespace-name)
  (let ((sdef (symbol->string def))
        (sname (if (symbol? namespace-name) 
                   (symbol->string namespace-name)
                   namespace-name)))
    (let ((prefix (string-append sname "_")))
      (if (string-starts-with? sdef prefix) def
          (string->symbol (string-append prefix sdef))))))

(define (get-import-name def prefix)
  (if prefix
      (get-name-from-namespace def prefix)
      def))
  
(define (update-namespace-defs-list! name)
    (if (scm-not (scm-memq name (scm-cdar *namespaces*)))
        (set-cdr! (scm-car *namespaces*) (scm-cons name (scm-cdar *namespaces*)))))

(define (add-namespace-prefix name)
  (if (null? *namespaces*)
      name
      (let ((sname (if (symbol? name) name (string->symbol name))))
        (update-namespace-defs-list! sname)
        (string-append (symbol->string (scm-caar *namespaces*)) "_" name))))

(define (add-def-to-namespace expr top)
  (if top
      (if (and (pair? expr) (scm-not (null? expr)))
          (cond ((and (or (scm-eq? (scm-car expr) 'define)
                          (scm-eq? (scm-car expr) 'define-structure))
                      (scm-not (null? *namespaces*)))
                 (let ((snamespace-name (symbol->string (scm-caar *namespaces*)))
                       (svar (symbol->string (scm-cadr expr))))
                   (update-namespace-defs-list! (scm-cadr expr))
                   (scm-append `(,(scm-car expr) ,(string->symbol (string-append snamespace-name "_" svar)))
                           (intern-to-top-namespace (cddr expr) top))))
                ((scm-eq? (scm-car expr) 'begin)
                 (let loop ((exprs (scm-cdr expr))
                            (result '()))
                   (if (null? exprs)
                       (scm-append (scm-list 'begin) (scm-reverse result))
                       (loop (scm-cdr exprs) (scm-cons (add-def-to-namespace (scm-car exprs) top) result)))))
                (else
                 (intern-to-top-namespace expr top)))
          (intern-to-top-namespace expr top))
      expr))

(define (intern-to-top-namespace expr top #!optional new-def)
  (if top
      (if (null? *namespaces*) expr
          (let ((n (scm-caar *namespaces*))
                (defs (scm-cdar *namespaces*)))
            (intern-to-namespace expr n (if new-def (scm-cons new-def defs) defs))))
      expr))

(define (let-vars bindings)
  (let loop ((bindings bindings)
             (vars '()))
    (if (null? bindings) 
        (scm-reverse vars)
        (loop (scm-cdr bindings) (scm-cons (scm-caar bindings) vars)))))

(define (flatten-vars vars)
  (let loop ((vars vars)
             (result '()))
    (if (null? vars)
        (scm-reverse result)
        (if (pair? (scm-car vars))
            (loop (scm-cdr vars) (scm-cons (scm-caar vars) result))
            (loop (scm-cdr vars) (scm-cons (scm-car vars) result))))))

(define (filtered-defs defs vars)
  (let ((vars (flatten-vars vars)))
    (let loop ((defs defs)
               (result '()))
      (if (null? defs)
          (scm-reverse result)
          (if (scm-memq (scm-car defs) vars)
              (loop (scm-cdr defs) result)
              (loop (scm-cdr defs) (scm-cons (scm-car defs) result)))))))
   
(define (intern-to-namespace-let-vars bindings namespace-name defs)
  (let loop ((bindings bindings)
             (result '()))
    (if (null? bindings)
        (scm-reverse result)
        (loop (scm-cdr bindings) 
              (scm-cons (scm-list 
                     (scm-caar bindings) 
                     (intern-to-namespace 
                      (scm-cadar bindings)
                      namespace-name defs))
                    result)))))

(define (intern-to-namespace expr namespace-name defs)
  (cond ((or (null? expr)
             (boolean? expr)
             (number? expr)
             (char? expr)
             (void? expr)
             (keyword? expr)
             (string? expr))
         expr)
        ((symbol? expr)
         (let ((name (scm-memq expr defs)))
           (if (scm-not name) expr
               (get-name-from-namespace (scm-car name) namespace-name))))
        (else (let ((sym (scm-car expr)))
                (cond ((or (scm-eq? sym 'let) (scm-eq? sym 'letrec) (scm-eq? sym 'let*))
                       (let* ((named-let (symbol? (scm-cadr expr)))
                              (let-expr (if named-let (scm-list sym (scm-cadr expr))
                                            (scm-list sym))))
                         (let ((vals (let ((v (intern-to-namespace-let-vars
                                               (if named-let (scm-caddr expr) (scm-cadr expr))
                                               namespace-name
                                               defs)))
                                       (if (null? v) v (scm-list v)))))
                           (scm-append let-expr (if (null? vals) (scm-list vals) vals)
                                   (intern-to-namespace 
                                    (if named-let (cdddr expr) (cddr expr))
                                    namespace-name
                                    (if (null? vals) defs (filtered-defs defs (let-vars (scm-car vals)))))))))
                      ((scm-eq? sym 'lambda)
                       (scm-append (scm-list sym (scm-cadr expr))
                               (intern-to-namespace
                                (cddr expr)
                                namespace-name
                                (filtered-defs defs (scm-cadr expr)))))
                      ((scm-eq? sym 'define)
                       (scm-append (scm-list sym (scm-cadr expr))
                               (intern-to-namespace
                                (cddr expr)
                                namespace-name
                                defs)))
                      ((scm-eq? sym 'set!)
                       (scm-append (scm-list sym (intern-to-namespace (scm-cadr expr) namespace-name defs))
                               (intern-to-namespace 
                                (cddr expr) 
                                namespace-name
                                defs)))
                      (else (let ((a (intern-to-namespace sym namespace-name defs))
                                  (b (intern-to-namespace (scm-cdr expr) namespace-name defs)))
                              (scm-cons a b))))))))

(define (add-to-declared-imported! name)
  (if (scm-not (in-declared-imports? name))
      (set! *declared-imported* (scm-cons name *declared-imported*)))
  `(define ,name *void*))

(define (in-declared-imports? name) (scm-memq name *declared-imported*))

(define (declare-imported names)
  (if (valid-identifier? names)
      (add-to-declared-imported! names)
      (let loop ((names names)
                 (expr '()))
        (if (null? names)
            (scm-append '(begin) (scm-reverse expr))
            (loop (scm-cdr names) (scm-cons (add-to-declared-imported! (scm-car names)) expr))))))

