(define (syntax-tokens tokenizer)
  (let loop ((token (tokenizer 'next))
             (tokens '())
             (parens 1))
    (cond
     ((eq? token '*open-brace*)
      (loop (tokenizer 'next)
            (scm-cons token tokens)
            (+ parens 1)))
     ((eq? token '*close-brace*)
      (cond
       ((eq? 1 parens)
        (loop #f tokens 0))
       (else
        (loop (tokenizer 'next)
              (scm-cons token tokens)
              (- parens 1)))))
     ((zero? parens)
      (scm-reverse tokens))
     ((< parens 0)
      (parser-error tokenizer "mismatched curley braces"))
     (else
      (loop (tokenizer 'next)
            (scm-cons token tokens)
            parens)))))

(define (syntax-var? p)
  (and (symbol? p)
       (char=? #\$ (string-ref (symbol->string p) 0))))

(define (extract-syntax-params tokens)
  (let loop ((tokens tokens) (params '()))
    (if (null? tokens)
        (scm-reverse params)
        (loop (scm-cdr tokens)
              (if (syntax-var? (scm-car tokens))
                  (scm-cons (scm-car tokens) params)
                  params)))))

(define *syntax-contexts* (scm-list (make-table)))

(define (push-syntax-context!)
  (set! *syntax-contexts*
        (scm-cons (make-table) *syntax-contexts*)))

(define (pop-syntax-context!)
  (set! *syntax-contexts* (scm-cdr *syntax-contexts*)))

(define (add-syntax! name tokens)
  (table-set! (scm-car *syntax-contexts*) name tokens))

(define (fetch-syntax name)
  (let loop ((contexts *syntax-contexts*))
    (if (null? contexts)
        #f
        (let ((s (table-ref (scm-car contexts) name #f)))
          (if s s (loop (scm-cdr contexts)))))))

(define (syntax-body-expr tokenizer)
  (tokenizer 'syntax-mode-on)
  (with-exception-catcher
   (lambda (e)
     (tokenizer 'syntax-mode-off)
     (scm-raise e))
   (lambda ()
     (let ((expr (func-body-expr tokenizer '())))
       (tokenizer 'syntax-mode-off)
       expr))))

(define (declare-syntax-stmt tokenizer)
  (let ((name (tokenizer 'next)))
    (if (scm-not (valid-identifier? name))
        (parser-error tokenizer "invalid syntax keyword"))
    (if (scm-not (eq? '*open-brace* (tokenizer 'next)))
        (parser-error tokenizer "expected opening curley brace"))
    (let ((tokens (syntax-tokens tokenizer))
          (body-expr (syntax-body-expr tokenizer)))
      (add-syntax! name tokens)
      `(define ,name (lambda ,(extract-syntax-params tokens) ,body-expr)))))
                                                             
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
                      (parser-error tokenizer "unexpected token in custom syntax")))))))
