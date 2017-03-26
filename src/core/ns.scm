;; Copyright (c) 2013-2017 by Vijay Mathew Pandyalakal, All Rights Reserved.

(define ns-tag '*-namespace-*)

(define (namespace? obj)
  (and (scm-not (null? obj))
       (list? obj)
       (pair? (scm-car obj))
       (eq? (scm-caar obj) ns-tag)))

(define (namespace-name ns)
  (scm-cdr (scm-car ns)))

(define (namespace-defs ns)
  (scm-cdr ns))

(define (namespace-binding ns name)
  (let ((b (scm-assq name (namespace-defs ns))))
    (if b
        (scm-cdr b)
        (scm-error "binding not found in namespace" name))))

(define (namespace-vars ns)
  (let ((defs (namespace-defs ns)))
    (scm-map scm-car defs)))

(define (assert-namespace-def defs name)
  (let loop ((defs defs))
    (cond ((null? defs)
           (scm-error "definition not found in namespace" name))
          ((eq? name (scm-car defs))
           #t)
          (else
           (loop (scm-cdr defs))))))

(define (namespace-filter-import-names defs import-names)
  (if (scm-not import-names)
      defs
      (let loop ((import-names import-names)
                 (filtered-defs '()))
        (cond ((null? import-names)
               filtered-defs)
              (else
               (let ((in (let ((p (scm-car import-names)))
                           (if (pair? p)
                               (scm-car p)
                               p))))
                 (assert-namespace-def defs in)
                 (loop (scm-cdr import-names)
                       (scm-cons in filtered-defs))))))))

(define (namespace-def-rename name import-names)
  (if (scm-not import-names)
      name
      (let loop ((import-names import-names))
        (cond ((null? import-names)
               name)
              (else
               (let ((p (scm-car import-names)))
                 (let ((in (if (pair? p) (scm-car p) p))
                       (rn (if (pair? p) (scm-cdr p) name)))
                   (if (eq? in name)
                       rn
                       (loop (scm-cdr import-names))))))))))
             
(define (make-namespace-import-all-stmt obj import-names)
  `(let loop ((defs (namespace-filter-import-names
                     (namespace-vars ,obj) ,import-names))
              (bindings '()))
     (if (null? defs)
         (scm-append (scm-list 'begin) bindings)
         (loop (scm-cdr defs)
               (let ((v (namespace-def-rename (scm-car defs) ,import-names)))
                 (scm-cons (scm-list 'define v
                                     (scm-list 'namespace-binding ',obj
                                               (scm-list 'quote v)))
                           bindings))))))
     
