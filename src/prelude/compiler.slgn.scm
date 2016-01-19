;; Copyright (c) 2013-2016 by Vijay Mathew Pandyalakal, All Rights Reserved.

(define *compiler-log* #f)

(define (compiler-log)
  (set! *compiler-log* (not *compiler-log*)))

(define (eliminate-voids exprs)
  (if (list? exprs)
      (filter (lambda (x) (not (void? x))) exprs drill: #t)
      exprs))

(define (compile->scheme tokenizer)
  (let loop ((v (eliminate-voids (slogan tokenizer)))
             (exprs '()))
    (if *compiler-log* (begin (scm-display v) (scm-newline)))
    (if (not (eof-object? v))
        (if (not (void? v)) 
            (loop (slogan tokenizer)
                  (scm-cons v exprs))
            (loop (slogan tokenizer) exprs))
        (scm-reverse exprs))))

(define (read-script-file script-name)
  (call-with-input-file (add-slgn-extn script-name)
    (lambda (port)
      (with-output-to-string 
        '()
        (lambda ()
          (let loop ((c (read-char port)))
            (if (not (eof-object? c))
                (begin (write-char c)
                       (loop (read-char port))))))))))

(define (compile-slgn-script->scm-script script-name 
                                         out-file-name
					 assemble exe)
  (call-with-output-file out-file-name
    (lambda (out-port)
      (let ((program-text (read-script-file script-name)))
        (let loop ((exprs (compile->scheme (make-tokenizer 
                                            (open-input-string program-text) 
                                            program-text
                                            compile-mode: (or assemble exe)))))
          (if (not (null? exprs))
              (begin (scm-write (scm-car exprs) out-port)
                     (scm-newline out-port)
                     (loop (scm-cdr exprs)))))))))

(define (compile script-name #!key assemble exe
		 ld_options cc_options output
                 (exception_handler display-exception))
  (let ((is-scm (string-ends-with? script-name *scm-extn*)))
    (with-exception-catcher
     exception_handler
     (lambda ()
       (let ((out-file-name 
              (if is-scm script-name (string-append script-name *scm-extn*))))
	 (if (not is-scm) 
             (compile-slgn-script->scm-script 
              script-name out-file-name 
              assemble exe))
	 (if (or assemble exe)
	     (let ((build-cmd 
                    (if exe 
                        (string-append *gsc-compiler* " -cc-options " 
                                       (if cc_options
                                           (string-append "\"" *default-cc-options* " " cc_options "\"")
                                           (string-append "\"" *default-cc-options* "\""))
                                       " -o " (if output output (path-strip-extension script-name))
                                       " -ld-options "
                                       (if ld_options
                                           (string-append "\"" *default-ld-options* " " ld_options "\"")
                                           (string-append "\"" *default-ld-options* "\""))
                                       " -exe "
                                       (string-append *prelude-root* "/*.scm ")
                                       out-file-name)
                        (string-append *gsc-compiler* " " out-file-name))))
	       (if (zero? (shell-command build-cmd))
		   (begin (delete-file out-file-name)
			  #t)
		   #f))
	     #t))))))

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
             (not (char=? (string-ref s 0) #\")))
        (string-for-each 
         (lambda (c)
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
                  (set! scount (- scount 1)))))
         s))
    (if (< bcount 0) (error "misplaced closing brace."))
    (if (< pcount 0) (error "misplaced closing parenthesis."))
    (if (< scount 0) (error "misplaced closing bracket."))    
    (and (zero? bcount)
         (zero? pcount)
         (zero? scount))))

(define (repl-exception-handler ex)
  (scm-display "error: ")
  (let ((s (open-output-string)))
    (show_exception ex s)
    (let loop ((lines (string_split (get_output_string s) #\newline)))
      (if (not (null? lines))
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
                           (let loop ((expr (slogan tokenizer)))
			     (if *compiler-log* (begin (scm-display expr) (scm-newline)))
                             (if (not (eof-object? (tokenizer 'peek)))
                                 (begin (eval expr)
                                        (loop (slogan tokenizer)))
                                 (eval expr)))))
                        (else (show-waiting-prompt prompt)
                              (loop (string-append 
                                     line 
                                     (read-line port #\newline #t))))))))
       (if (and (not (void? val)))
           (begin (slgn-display val)
                  (scm-newline))))))
  (slogan-repl port prompt: prompt))

(define (run-slgn-script script-name) 
  (let ((program-text (read-script-file script-name)))
    (let loop ((exprs (compile->scheme (make-tokenizer 
                                        (open-input-string program-text) 
                                        program-text)))
               (val #!void))
      (if (not (null? exprs))
          (loop (scm-cdr exprs) (eval (scm-car exprs)))
          (if (not (void? val))
              (begin (slgn-display val)
                     (scm-newline)))))))

