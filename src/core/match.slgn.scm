;; Copyright (c) 2013-2016 by Vijay Mathew Pandyalakal, All Rights Reserved.

(define-structure record-pattern name members)

(define (normalize-list-for-matching lst)
  (if (and (list? lst)
           (scm-not (null? lst)))
      (if (scm-eq? (scm-car lst) 'scm-list)
          (scm-cdr lst)
          (list->record-pattern lst))
      lst))

(define (list->record-pattern lst)
  (if (symbol? (scm-car lst))
      (let ((name (symbol->string (scm-car lst))))
        (if (char=? #\+ (string-ref name 0))
            (let loop ((name (substring name 1 (string-length name)))
                       (lst (scm-cdr lst))
                       (members '()))
              (cond ((null? lst)
                     (make-record-pattern name (scm-reverse members)))
                    (else 
                     (if (keyword? (scm-car lst))
                         (loop name (scm-cddr lst)
                               (scm-cons (scm-cons (keyword->string (scm-car lst)) (scm-cadr lst)) members))
                         (loop name (scm-cdr lst)
                               (scm-cons (scm-car lst) members))))))
            lst))
      lst))

(define (scm-cons? pattern)
  (and (list? pattern)
       (scm-eq? (scm-car pattern) 'scm-cons)))

(define-structure pattern-vars bindings)

(define (match-pattern pattern consequent)
  (let ((bindings (make-pattern-vars '())))
    `(if (unbound? *result*)
         (begin
           ,(if (scm-eq? pattern 'else)
                `(set! *match-found* #t)
                (match-pattern-helper pattern bindings))
           (set! *result* (if *match-found* 
                              ,(if (scm-not (null? (pattern-vars-bindings bindings)))
                                   `(let ,(pattern-vars-bindings bindings)
                                      ,(expand-consequent pattern consequent))
                                   (expand-consequent pattern consequent))
                              '*unbound*))))))

(define *vector-patterns* '(scm-vector
                            scm-u8vector
                            scm-s8vector
                            bit_array
                            make-set
                            scm-u16vector
                            scm-s16vector
                            scm-u32vector
                            scm-s32vector
                            scm-u64vector
                            scm-s64vector
                            scm-f32vector
                            scm-f64vector))

(define (vector-pattern? fname) (scm-memq fname *vector-patterns*))

(define (vector-test-fn fname)
  (case fname
    ((scm-vector) 'vector?)
    ((scm-u8vector) 'u8vector?)
    ((scm-s8vector) 's8vector?)
    ((bit_array) 'is_bit_array)
    ((make-set) 'set-type?)
    ((scm-u16vector) 'u16vector?)
    ((scm-s16vector) 's16vector?)
    ((scm-u32vector) 'u32vector?)
    ((scm-s32vector) 's32vector?)
    ((scm-u64vector) 'u64vector?)
    ((scm-s64vector) 's64vector?)
    ((scm-f32vector) 'f32vector?)
    ((scm-f64vector) 'f64vector?)
    (else (scm-error "Invalid vector constructor." fname))))

(define (vector-to-list-fn fname)
    (case fname
    ((scm-vector) 'vector->list)
    ((scm-u8vector) 'u8vector->list)
    ((scm-s8vector) 's8vector->list)
    ((bit_array) 'bit_array_to_list)
    ((make-set) 'set->list)
    ((scm-u16vector) 'u16vector->list)
    ((scm-s16vector) 's16vector->list)
    ((scm-u32vector) 'u32vector->list)
    ((scm-s32vector) 's32vector->list)
    ((scm-u64vector) 'u64vector->list)
    ((scm-s64vector) 's64vector->list)
    ((scm-f32vector) 'f32vector->list)
    ((scm-f64vector) 'f64vector->list)
    (else (scm-error "Invalid vector constructor." fname))))

(define (hashtable-pattern? p)
  (eq? (scm-car p) 'make-equal-hashtable))

(define (match-hashtable-pattern pattern bindings)
  (let* ((pkey-vals (scm-cdadr pattern))
         (pkeys-1 (scm-map scm-cadr pkey-vals))
         (pvals (scm-map scm-caddr pkey-vals)))
    (let loop ((pkeys pkeys-1) (pvals pvals) (expr '()))
      (if (null? pkeys)
          `(if (and (is_hashtable *value*)
               (for_all (lambda (k) (hashtable_contains *value* k)) ,(scm-append '(scm-list) pkeys-1)))
               (begin
                 (set! *match-found* #t)
                 ,@(scm-reverse expr))
               (set! *match-found* #f))
          (let ((pk (scm-car pkeys)) (pv (scm-car pvals)))
            (loop
             (scm-cdr pkeys) (scm-cdr pvals)
             (scm-cons
              `(begin
                 (let ((*v* (hashtable_at *value* ,pk)))
                   ,(cond
                     ((symbol? pv)
                      (pattern-vars-bindings-set! bindings
                                                  (scm-cons (scm-list pv #f) (pattern-vars-bindings bindings)))
                      `(set! *match-found* #t))
                     (else
                      `(let ((*value* *v*))
                         ,(match-pattern-helper pv bindings))))))
              expr)))))))

(define (set-pattern? p)
  (eq? (scm-car p) 'make-set))

(define (match-list-pattern pattern bindings length-test?)
  (let ((pattern-length (scm-length pattern)))
    `(if ,(if length-test?
              `(and (list? *value*)
                    (= ,pattern-length (scm-length *value*)))
              #t)
         (begin (let ((*value* (scm-first *value*)))
                  ,(match-pattern-helper (scm-car pattern) bindings #t))
                (if *match-found*
                    (let ((*value* (scm-rest *value*)))
                      ,(match-pattern-helper (scm-cdr pattern) bindings #f))))
         (set! *match-found* #f))))

(define (match-vector-pattern fname pattern bindings)
  `(if (,(vector-test-fn fname) *value*)
       (let ((*value* (,(vector-to-list-fn fname) *value*)))
         ,(match-list-pattern pattern bindings #t))
       (set! *match-found* #f)))

(define (match-record-pattern pattern bindings)
  (let ((predic (string->symbol (string-append (record-pattern-name pattern) "?"))))
    (let ((prefix `(if (,predic *value*))))
      (let loop ((members (record-pattern-members pattern))
                 (conds '()))
        (cond ((null? members)
               (let ((body '(set! *match-found* #t)))
                 (scm-append
                  (scm-append prefix (scm-list (if (null? conds) 
                                           `(if #t ,body)
                                           `(if (and ,@(scm-reverse conds)) 
                                                ,body 
                                                (set! *match-found* #f)))))
                  (scm-list '(set! *match-found* #f)))))
              ((symbol? (scm-car members))
               (let ((s (scm-car members)))
                 (if (scm-not (scm-eq? s '_))
                     (pattern-vars-bindings-set! bindings (scm-cons (scm-list s #f) (pattern-vars-bindings bindings)))))
               (loop (scm-cdr members) conds))
              ((pair? (scm-car members))
               (let ((accessor (string->symbol (string-append (record-pattern-name pattern)
                                                              "-" (scm-caar members)))))
                 (loop (scm-cdr members) (scm-cons `(equal? ,(scm-cdar members) (,accessor *value*)) conds))))
              (else
               (scm-error "Invalid record pattern: " pattern)))))))

(define (match-pattern-helper pattern bindings #!optional (length-test? #t))
  (set! pattern (normalize-list-for-matching pattern))
  (cond ((null? pattern)
         `(if (null? *value*)
              (set! *match-found* #t)
              (set! *match-found* #f)))
        ((scm-cons? pattern)
         (set! pattern (scm-cdr pattern))
         `(if (pair? *value*)
              (begin (let ((*value* (scm-first *value*)))
                       ,(match-pattern-helper (scm-car pattern) bindings))
                     (if *match-found*
                         (let ((*value* (scm-rest *value*)))
                           ,(match-pattern-helper (scm-cadr pattern) bindings))))
              (set! *match-found* #f)))
        ((list? pattern)
          (cond
            ((scm-eq? (scm-car pattern) 'quote)
              `(if (equal? ,pattern *value*)
                 (set! *match-found* #t)
                 (set! *match-found* #f)))
            ((vector-pattern? (scm-car pattern))
              (match-vector-pattern (scm-car pattern) (scm-cdr pattern) bindings))
            ((hashtable-pattern? pattern)
             (match-hashtable-pattern pattern bindings))
            (else
             (match-list-pattern pattern bindings length-test?))))
        ((record-pattern? pattern)
         (match-record-pattern pattern bindings))
        ((symbol? pattern)
         (if (scm-not (scm-eq? pattern '_))
           (pattern-vars-bindings-set! bindings (scm-cons (scm-list pattern #f) (pattern-vars-bindings bindings))))
         `(set! *match-found* #t))
        (else `(if (equal? ,pattern *value*)
                   (set! *match-found* #t)
                   (set! *match-found* #f)))))

(define (expand-vector-consequent fname pattern consequent)
  `(begin (set! *value* (if (,(vector-test-fn fname) *value*)
                            (,(vector-to-list-fn fname) *value*)
                            *value*))
          (let ((*rest* (scm-rest *value*)))
            (set! *value* (scm-first *value*))
            ,(expand-consequent (scm-car pattern) #f)
            (set! *value* *rest*)
            ,(expand-consequent (scm-cdr pattern) consequent))))

(define (expand-hashtable-consequent pattern consequent)
  (let* ((pkey-vals (scm-cdadr pattern))
         (pkeys (scm-map scm-cadr pkey-vals))
         (pvals (scm-map scm-caddr pkey-vals)))
    (let loop ((pkeys pkeys) (pvals pvals) (expr '(begin)))
      (if (null? pkeys)
        (scm-append (scm-reverse expr) (scm-list consequent))
        (loop (scm-cdr pkeys) (scm-cdr pvals)
          (scm-cons
            `(let ((*orig-value* *value*))
               (let ((*value* (hashtable_at *orig-value* ,(scm-car pkeys))))
                 ,(expand-consequent (scm-car pvals) #f)))
            expr))))))

(define (expand-consequent pattern consequent)
  (set! pattern (normalize-list-for-matching pattern))
  (cond ((or (null? pattern) (scm-eq? pattern 'else))
         consequent)
        ((scm-cons? pattern)
         (set! pattern (scm-cdr pattern))
         `(let ((*rest* (scm-rest *value*)))
            (set! *value* (scm-first *value*))
            ,(expand-consequent (scm-car pattern) #f)
            (set! *value* *rest*)
            ,(expand-consequent (scm-cadr pattern) consequent)))
        ((list? pattern)
          (cond
            ((scm-eq? (scm-car pattern) 'quote)
              consequent)
            ((vector-pattern? (scm-car pattern))
             (expand-vector-consequent (scm-car pattern) (scm-cdr pattern) consequent))
            ((hashtable-pattern? pattern)
              (expand-hashtable-consequent pattern consequent))
            (else
              `(let ((*rest* (scm-rest *value*)))
                 (set! *value* (scm-first *value*))
                 ,(expand-consequent (scm-car pattern) #f)
                 (set! *value* *rest*)
                 ,(expand-consequent (scm-cdr pattern) consequent)))))
        ((record-pattern? pattern)
         (expand-rec-consequent pattern consequent))
        ((symbol? pattern)
         (if (scm-eq? pattern '_)
             consequent
             `(begin (set! ,pattern *value*)
                     ,consequent)))
        (else consequent)))

(define (expand-rec-consequent pattern consequent)
  (let loop ((members (record-pattern-members pattern))
             (i 0)
             (bindings '()))
    (cond ((null? members)
           (if (null? bindings)
               consequent
               `(begin ,@(scm-reverse bindings) ,consequent)))
          ((symbol? (scm-car members))
           (if (scm-eq? (scm-car members) '_)
               (loop (scm-cdr members) (+ i 1) bindings)
               (let ((accessor (string->symbol (string-append (record-pattern-name pattern)
                                                              "-" (number->string i)))))
                 (loop (scm-cdr members) (+ i 1) (scm-cons `(set! ,(scm-car members) (,accessor *value*)) bindings)))))
          (else (loop (scm-cdr members) (+ i 1) bindings)))))
