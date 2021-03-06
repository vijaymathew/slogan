;; Copyright (c) 2013-2019 by Vijay Mathew Pandyalakal, All Rights Reserved.

(define (syntax-tokens tokenizer)
  (let loop ((token (tokenizer 'next))
             (tokens '()))
    (if (eq? token '*syntax-inserter*)
        (scm-reverse tokens)
        (loop (tokenizer 'next)
              (scm-cons token tokens)))))

(define (syntax-var? p)
  (and (symbol? p)
       (let ((c (string-ref (symbol->string p) 0)))
         (char=? #\$ c))))

(define (extract-syntax-params tokens)
  (let loop ((tokens tokens) (params '()))
    (if (null? tokens)
        (scm-reverse params)
        (loop (scm-cdr tokens)
              (if (syntax-var? (scm-car tokens))
                  (scm-cons (scm-car tokens) params)
                  params)))))

(define *syntax-contexts* (scm-list (make-table)))
(define *macro-contexts* (scm-list (make-table)))

(define (push-syntax-context!)
  (set! *syntax-contexts*
        (scm-cons (make-table) *syntax-contexts*))
  (set! *macro-contexts*
        (scm-cons (make-table) *macro-contexts*)))

(define (pop-syntax-context!)
  (set! *syntax-contexts* (scm-cdr *syntax-contexts*))
  (set! *macro-contexts* (scm-cdr *macro-contexts*)))

(define (add-syntax! name tokens)
  (table-set! (scm-car *syntax-contexts*) name tokens))

(define (add-macro! name tokens parsers fn-expr)
  (table-set! (scm-car *macro-contexts*) name
              (scm-cons (scm-cons tokens parsers) (scm-eval fn-expr))))

(define (fetch-syntax-macro name ctx)
  (let loop ((contexts ctx))
    (if (null? contexts)
        #f
        (let ((s (table-ref (scm-car contexts) name #f)))
          (if s s (loop (scm-cdr contexts)))))))

(define (fetch-syntax name) (fetch-syntax-macro name *syntax-contexts*))
(define (fetch-macro name) (fetch-syntax-macro name *macro-contexts*))

(define (syntax-body-expr tokenizer syntax?)
  (if syntax? (tokenizer 'syntax-mode-on))
  (with-exception-catcher
   (lambda (e)
     (tokenizer 'syntax-mode-off)
     (scm-raise e))
   (lambda ()
     (let ((expr (func-body-expr tokenizer '())))
       (tokenizer 'syntax-mode-off)
       expr))))

(define (normalize-syntax-var-parsers parsers)
  (scm-map (lambda (p)
             (if (scm-not (eq? 'scm-cons (scm-car p)))
                 (scm-error "invalid parser specification, expected a pair"))
             (scm-cons (scm-cadr p) (scm-cddr p)))
           parsers))

(define (syntax-var-parsers tokenizer)
  (cond
   ((eq? '*comma* (tokenizer 'peek))
    (tokenizer 'next)
    (let ((parsers (scm-expression tokenizer)))
      (if (and (scm-not (list? parsers))
               (scm-not (eq? 'scm-list (scm-car parsers))))
          (parser-error tokenizer "expected list of parsers")
          (normalize-syntax-var-parsers (scm-cdr parsers)))))
   (else '())))

(define (var-parser var parsers)
  (let ((p (scm-assq var parsers)))
    (if p (scm-eval (scm-cadr p)) #f)))

(define (declare-syntax-stmt tokenizer)
  (let ((name (tokenizer 'next)))
    (if (scm-not (valid-identifier-exclude-syntax? name))
        (parser-error tokenizer "invalid syntax name"))
    (let ((tokens (syntax-tokens tokenizer))
          (body-expr (syntax-body-expr tokenizer #t)))
      (add-syntax! name tokens)
      `(define ,name (lambda ,(extract-syntax-params tokens) ,body-expr)))))

(define (declare-macro-stmt tokenizer)
  (let ((name (tokenizer 'next)))
    (if (scm-not (valid-identifier-exclude-syntax? name))
        (parser-error tokenizer "invalid macro keyword"))
    (let ((tokens (syntax-tokens tokenizer))
          (body-expr (syntax-body-expr tokenizer #f))
          (parsers (syntax-var-parsers tokenizer)))
      (add-macro! name tokens parsers `(lambda ,(extract-syntax-params tokens) ,body-expr)))))
                                                             
(define (parse-syntax-call-expr name tokens tokenizer)
  (tokenizer 'next)
  (let loop ((tokens tokens) (args '()))
    (if (null? tokens)
        `(,name ,@(scm-reverse args))
        (loop (scm-cdr tokens)
              (if (syntax-var? (scm-car tokens))
                  (scm-cons `(lambda () ,(func-body-expr tokenizer '())) args)
                  (if (equal? (scm-car tokens) (tokenizer 'next))
                      args
                      (parser-error tokenizer "unexpected token in syntax call")))))))

(define (parse-macro-call-expr tokens/fn-expr tokenizer)
  (tokenizer 'next)
  (let ((parsers (scm-cdar tokens/fn-expr)))
    (let loop ((tokens (scm-caar tokens/fn-expr))
               (args '()))
      (if (null? tokens)
          (let ((fn (scm-cdr tokens/fn-expr)))
            (scm-apply fn (scm-reverse args)))
          (loop (scm-cdr tokens)
                (if (syntax-var? (scm-car tokens))
                    (let ((parser (var-parser (scm-car tokens) parsers)))
                      (scm-cons (if parser (parser tokenizer) (func-body-expr tokenizer '())) args))
                    (if (equal? (scm-car tokens) (tokenizer 'next))
                        args
                        (parser-error tokenizer "unexpected token in macro call"))))))))
