;; Copyright (c) 2013-2017 by Vijay Mathew Pandyalakal, All Rights Reserved.

(define ns-tag '*-namespace-*)

(define (make-namespace-expr name body)
  (let ((defs (extract-module-defs body '())))
    `(define ,name
       (scm-cons
        (scm-cons ns-tag ',name)
        (let ()
          ,@(scm-cdr body)
          (list->hashtable
           (scm-list
            ,@(scm-map
               (lambda (def)
                 (scm-list
                  'scm-cons
                  (scm-list 'quote def) def))
               defs))))))))

(define (namespace? obj)
  (and (pair? obj)
       (pair? (scm-car obj))
       (eq? (scm-caar obj) ns-tag)))

(define (namespace-name ns)
  (scm-cdr (scm-car ns)))

(define (namespace-defs ns)
  (scm-cdr ns))

