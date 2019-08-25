;; Copyright (c) 2013-2019 by Vijay Mathew Pandyalakal, All Rights Reserved.

(define *compiler-log* #f)

(define (compiler-log)
  (set! *compiler-log* (scm-not *compiler-log*)))

(define (eliminate-voids exprs)
  (if (list? exprs)
      (scm-filter (lambda (x) (scm-not (void? x))) exprs drill: #t)
      exprs))

(define (compile->scheme tokenizer)
  (let loop ((v (eliminate-voids (scm-slogan tokenizer)))
             (exprs '()))
    (if *compiler-log* (begin (scm-display v) (scm-newline)))
    (if (scm-not (eof-object? v))
          (if (scm-not (void? v)) 
              (loop (scm-slogan tokenizer)
                    (scm-cons v exprs))
              (loop (scm-slogan tokenizer) exprs))
	  (scm-reverse exprs))))

(define (read-script-file script-name)
  (call-with-input-file (add-slgn-extn script-name)
    (lambda (port)
      (with-output-to-string 
        '()
        (lambda ()
          (let loop ((c (read-char port)))
            (if (scm-not (eof-object? c))
                (begin (write-char c)
                       (loop (read-char port))))))))))

(define (compile-slgn-script->scm-script script-name out-file-name assemble exe)
  (call-with-output-file out-file-name
    (lambda (out-port)
      (let ((program-text (read-script-file script-name)))
        (let loop ((exprs (compile->scheme (make-tokenizer 
                                            (open-input-string program-text) 
                                            program-text
                                            compile-mode: (or assemble exe)))))
          (if (scm-not (null? exprs))
              (begin (scm-write (scm-car exprs) out-port)
                     (scm-newline out-port)
                     (loop (scm-cdr exprs)))))))))

(define (exe-build-command script-name cc-options ld-options output out-file-name)
  (string-append *gsc-compiler*
                 (if cc-options (string-append " -cc-options " cc-options) "")
                 " -o " (if output output (path-strip-extension script-name))
                 " -exe -l " (string-append *prelude-root* "/_slogan.c -ld-options ")
                 (if ld-options
                     (string-append "\"" *default-ld-options* " " ld-options "\"")
                     (string-append "\"" *default-ld-options* "\""))
                 " "
                 out-file-name))

(define (compile-exception-handler ex)
  (scm-raise ex))

(define (compile script-name #!key assemble exe
		 ld_options cc_options output
                 (exception_handler compile-exception-handler))
  (let ((is-scm (string-ends-with? script-name *scm-extn*)))
    (with-exception-catcher
     exception_handler
     (lambda ()
       (let ((out-file-name 
              (if is-scm script-name (string-append script-name *scm-extn*))))
	 (if (scm-not is-scm) 
             (compile-slgn-script->scm-script script-name out-file-name assemble exe))
	 (if (or assemble exe)
	     (let ((build-cmd 
                    (if exe
                        (exe-build-command script-name cc_options ld_options output out-file-name)
                        (string-append *gsc-compiler* " " out-file-name))))
               (scm-display build-cmd) (scm-newline)
	       (if (zero? (shell-command build-cmd))
		   (begin (delete-file out-file-name)
			  #t)
		   #f))
	     #t))))))

(define slgn-compile compile)

(define (show-waiting-prompt prompt)
  (if prompt
      (let loop ((len (scm-- (string-length prompt) 2))
                 (i 0))
        (if (scm-< len 0)
            (scm-display "> ")
            (if (scm-< i len)
                (begin (scm-display " ")
                       (loop len (scm-+ i 1)))
                (scm-display "> "))))))

(define pair-chars (list (cons #\] #\[)
			 (cons #\} #\{)
			 (cons #\) #\()))

(define pushable-chars (scm-map scm-cdr pair-chars))
(define poppable-chars (scm-map scm-car pair-chars))

(define (pop-char c stk)
  (let ((pc (scm-cdr (scm-assoc c pair-chars))))
    (if (scm-not (char=? pc (scm-car stk)))
	(scm-error (string-append "misplaced character - " (scm-string c))))
    (scm-cdr stk)))

(define (braces-matches? s)
    (if (and (scm-> (string-length s) 0)
             (scm-not (char=? (string-ref s 0) #\")))
        (let loop ((ss (string->list s))
		   (tokens-stack '())
		   (prev-c #\nul))
	  (if (scm-not (null? ss))
	      (let* ((c (scm-car ss))
		     (in-str? (and (char=? c #\") (scm-not (char=? prev-c #\\))))
		     (in-char? (char=? prev-c #\\)))
		(if (and (scm-not in-str?)
			 (scm-not in-char?))
		    (loop (scm-cdr ss)
			  (cond ((scm-member c pushable-chars)
				 (cons c tokens-stack))
				((scm-member c poppable-chars)
				 (pop-char c tokens-stack))
				(else tokens-stack))
			  c))))))
    #t)

(define (repl-exception-handler ex)
  (scm-display "error: ")
  (let ((s (open-output-string)))
    (show_exception ex s)
    (let loop ((lines (string-split (get-output-string s) #\newline)))
      (if (scm-not (null? lines))
          (begin (scm-display (scm-car lines))
                 (scm-newline)
                 (loop (scm-cdr lines)))))))

(define (slogan-repl port #!key (prompt "slogan> "))
  (if prompt (scm-display prompt))
  (with-exception-catcher
   repl-exception-handler
   (lambda ()
     (let ((val (let loop ((line (read-line port #\newline #t)))
                  (cond ((and (string-ends-with? (string-rtrim line) ";")
                              (braces-matches? line))
                         (let ((tokenizer (make-tokenizer (open-input-string line) 
                                                          line)))
                           (let loop ((expr (scm-slogan tokenizer)))
			     (if *compiler-log* (begin (scm-display expr) (scm-newline)))
                             (if (scm-not (eof-object? (tokenizer 'peek)))
                                 (begin (scm-eval expr)
                                        (loop (scm-slogan tokenizer)))
                                 (scm-eval expr)))))
                        (else (show-waiting-prompt prompt)
                              (loop (string-append 
                                     line 
                                     (read-line port #\newline #t))))))))
       (if (and (scm-not (void? val)))
           (begin (slgn-display val)
                  (scm-newline))))))
  (slogan-repl port prompt: prompt))

(define (run-slgn-script script-name) 
  (let ((program-text (read-script-file script-name)))
    (let loop ((exprs (compile->scheme (make-tokenizer 
                                        (open-input-string program-text) 
                                        program-text)))
               (val #!void))
      (if (scm-not (null? exprs))
          (loop (scm-cdr exprs) (scm-eval (scm-car exprs)))
          (if (scm-not (void? val))
              (begin (slgn-display val)
                     (scm-newline)))))))

;; meta-programming
(define (compiler-letfn-block bindings body)
  `(let ,bindings ,body))

(define (compiler-let-block bindings body)
  `(let* ,bindings ,body))

(define (compiler-letrec-block bindings body)
  `(letrec ,bindings ,body))

(define (compiler-named-let-block name bindings body)
  `(let ,name ,bindings ,body))

(define (compiler-when cond conseq)
  `(if ,cond ,conseq #f))

(define (compiler-if-else cond conseq alter)
  `(if ,cond ,conseq ,alter))

(define (compiler-if-else-multi conds-conseqs)
  `(cond ,@conds-conseqs))

(define (compiler-case expr conds-conseqs)
  `(case ,expr ,@conds-conseqs))

(define (compiler-code-block body)
  `(begin ,@body))

(define (compiler-fn-call fn-name args)
  `(,fn-name ,@args))

(define (compiler-fn args body)
  `(lambda ,args ,body))

(define (compiler-let var-name value)
  `(define ,var-name ,value))

(define (compiler-assignment var-name value)
  `(set! ,var-name ,value))

(define (compiler-parse-let-bindings tokenizer)
  (let-bindings tokenizer 'let))

(define (compiler-make-let-bindings-parser letkw)
  (lambda (tokenizer)
    (let-bindings tokenizer letkw)))

(define (compiler msg)
  (case msg
    ((letfn_) compiler-letfn-block)
    ((let_) compiler-let-block)
    ((letrec_) compiler-letrec-block)
    ((named_let) compiler-named-let-block)
    ((when_) compiler-when)
    ((if_) compiler-if-else)
    ((if_multi) compiler-if-else-multi)
    ((case_) compiler-case)
    ((block) compiler-code-block)
    ((call) compiler-fn-call)
    ((function_) compiler-fn)
    ((let_statement) compiler-let)
    ((assignment) compiler-assignment)
    ((let_bindings_parser) compiler-parse-let-bindings)
    ((make_bindings_parser) compiler-make-let-bindings-parser)
    (else (scm-error "invalid compiler message"))))
