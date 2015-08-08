;; Copyright (c) 2013-2014 by Vijay Mathew Pandyalakal, All Rights Reserved.

(define *namespaces* '())
(define *active-namespaces* (make-table))
(define *declared-imported* '())

(define (push-namespace name)
  (set! *namespaces* (cons (cons (add-parent-namespace-names name) 
                                 (existing-namespace-defs name)) 
                           *namespaces*)))

(define (pop-namespace)
  (if (not (null? *namespaces*))
      (let ((top (car *namespaces*)))
        (table-set! *active-namespaces* (car top) (cdr top))
        (set! *namespaces* (cdr *namespaces*)))))

(define (existing-namespace-defs name)
  (table-ref *active-namespaces* name '()))

(define (add-parent-namespace-names name)
  (if (null? *namespaces*) name
      (let loop ((namespaces (reverse *namespaces*))
                 (sname ""))
        (if (null? namespaces)
            (string->symbol (string-append sname (symbol->string name)))
            (loop (cdr namespaces)
                  (string-append sname (symbol->string (caar namespaces)) "_"))))))

(define (import-from-namespace name #!optional defs prefix)
  (let ((n (find-namespace name)))
    (if (and (not n) (not defs)) (error "namespace not found" name))
    (if (and n defs) (validate-import-names defs n))
    (let loop ((defs (if defs defs n))
               (imports '()))
      (if (null? defs)
          (cons 'begin imports)
          (let ((import-name (get-import-name (car defs) prefix)))
            (loop (cdr defs) (cons `(,(if (in-declared-imports? import-name) 'set! 'define)
                                    ,import-name
                                    ,(get-name-from-namespace (car defs) name))
                                 imports)))))))

(define (validate-import-names defs namespace)
  (let loop ((defs defs))
    (if (null? defs) #t
        (if (not (memq (car defs) namespace))
            (error "Name not found in namespace." (car defs))
            (loop (cdr defs))))))

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
    (if (not (memq name (cdar *namespaces*)))
        (set-cdr! (car *namespaces*) (cons name (cdar *namespaces*)))))

(define (add-namespace-prefix name)
  (if (null? *namespaces*)
      name
      (let ((sname (if (symbol? name) name (string->symbol name))))
        (update-namespace-defs-list! sname)
        (string-append (symbol->string (caar *namespaces*)) "_" name))))

(define (add-def-to-namespace expr top)
  (if top
      (if (and (pair? expr) (not (null? expr)))
          (cond ((and (or (eq? (car expr) 'define)
                          (eq? (car expr) 'define-structure))
                      (not (null? *namespaces*)))
                 (let ((snamespace-name (symbol->string (caar *namespaces*)))
                       (svar (symbol->string (cadr expr))))
                   (update-namespace-defs-list! (cadr expr))
                   (append `(,(car expr) ,(string->symbol (string-append snamespace-name "_" svar)))
                           (intern-to-top-namespace (cddr expr) top))))
                ((eq? (car expr) 'begin)
                 (let loop ((exprs (cdr expr))
                            (result '()))
                   (if (null? exprs)
                       (append (list 'begin) (reverse result))
                       (loop (cdr exprs) (cons (add-def-to-namespace (car exprs) top) result)))))
                (else
                 (intern-to-top-namespace expr top)))
          (intern-to-top-namespace expr top))
      expr))

(define (intern-to-top-namespace expr top)
  (if top
      (if (null? *namespaces*) expr
          (intern-to-namespace expr (caar *namespaces*) (cdar *namespaces*)))
      expr))

(define (let-vars bindings)
  (let loop ((bindings bindings)
             (vars '()))
    (if (null? bindings) 
        (reverse vars)
        (loop (cdr bindings) (cons (caar bindings) vars)))))

(define (filtered-defs defs vars)
  (let loop ((defs defs)
             (result '()))
    (if (null? defs)
        (reverse result)
        (if (memq (car defs) vars)
            (loop (cdr defs) result)
            (loop (cdr defs) (cons (car defs) result))))))
   
(define (intern-to-namespace-let-vars bindings namespace-name defs)
  (let loop ((bindings bindings)
             (result '()))
    (if (null? bindings)
        (reverse result)
        (loop (cdr bindings) 
              (cons (list 
                     (caar bindings) 
                     (intern-to-namespace 
                      (cadar bindings)
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
         (let ((name (memq expr defs)))
           (if (not name) expr
               (get-name-from-namespace (car name) namespace-name))))
        (else (let ((sym (car expr)))
                (cond ((or (eq? sym 'let) (eq? sym 'letrec) (eq? sym 'let*))
                       (let* ((named-let (symbol? (cadr expr)))
                              (let-expr (if named-let (list sym (cadr expr))
                                            (list sym))))
                         (let ((vals (let ((v (intern-to-namespace-let-vars
                                               (if named-let (caddr expr) (cadr expr))
                                               namespace-name
                                               defs)))
                                       (if (null? v) v (list v)))))
                           (append let-expr (if (null? vals) (list vals) vals)
                                   (intern-to-namespace 
                                    (if named-let (cdddr expr) (cddr expr))
                                    namespace-name
                                    (if (null? vals) defs (filtered-defs defs (let-vars (car vals)))))))))
                      ((eq? sym 'lambda)
                       (append (list sym (cadr expr))
                               (intern-to-namespace
                                (cddr expr)
                                namespace-name
                                (filtered-defs defs (cadr expr)))))
                      ((eq? sym 'define)
                       (append (list sym (cadr expr))
                               (intern-to-namespace
                                (cddr expr)
                                namespace-name
                                defs)))
                      ((eq? sym 'set!)
                       (append (list sym (intern-to-namespace (cadr expr) namespace-name defs))
                               (intern-to-namespace 
                                (cddr expr) 
                                namespace-name
                                defs)))
                      (else (let ((a (intern-to-namespace sym namespace-name defs))
                                  (b (intern-to-namespace (cdr expr) namespace-name defs)))
                              (cons a b))))))))

(define (add-to-declared-imported! name)
  (if (not (in-declared-imports? name))
      (set! *declared-imported* (cons name *declared-imported*)))
  `(define ,name *void*))

(define (in-declared-imports? name) (memq name *declared-imported*))

(define (declare-imported names)
  (if (valid-identifier? names)
      (add-to-declared-imported! names)
      (let loop ((names names)
                 (expr '()))
        (if (null? names)
            (append '(begin) (reverse expr))
            (loop (cdr names) (cons (add-to-declared-imported! (car names)) expr))))))
