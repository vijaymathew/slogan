;; Copyright (c) 2013-2017 by Vijay Mathew Pandyalakal, All Rights Reserved.

(define *compiler-log* #f)

(define (compiler-log)
  (set! *compiler-log* (scm-not *compiler-log*)))

(define (eliminate-voids exprs)
  (if (list? exprs)
      (filter (lambda (x) (scm-not (void? x))) exprs drill: #t)
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
      (let loop ((len (- (string-length prompt) 2))
                 (i 0))
        (if (< len 0)
            (scm-display "> ")
            (if (< i len)
                (begin (scm-display " ")
                       (loop len (+ i 1)))
                (scm-display "> "))))))

(define (braces-matches? s)
  (let ((bcount 0)
        (pcount 0)
        (scount 0))
    (if (and (> (string-length s) 0)
             (scm-not (char=? (string-ref s 0) #\")))
        (let ((in-str? #f) (prev-c #\nul))
          (string-for-each
           (lambda (c)
             (if (and (char=? c #\") (scm-not (char=? prev-c #\\)))
                 (set! in-str? (scm-not in-str?)))
             (set! prev-c c)
             (if (scm-not in-str?)
                 (cond ((char=? c #\{)
                        (set! bcount (+ bcount 1)))
                       ((char=? c #\})
                        (set! bcount (- bcount 1)))
                       ((char=? c #\()
                        (set! pcount (+ pcount 1)))
                       ((char=? c #\))
                        (set! pcount (- pcount 1)))
                       ((char=? c #\[)
                        (set! scount (+ scount 1)))
                       ((char=? c #\])
                        (set! scount (- scount 1))))))
           s)))
    (if (< bcount 0) (scm-error "misplaced closing brace"))
    (if (< pcount 0) (scm-error "misplaced closing parenthesis"))
    (if (< scount 0) (scm-error "misplaced closing bracket"))
    (and (zero? bcount)
         (zero? pcount)
         (zero? scount))))

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
