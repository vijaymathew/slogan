;; Copyright (c) 2013-2014 by Vijay Mathew Pandyalakal, All Rights Reserved.

(define (compile->scheme tokenizer)
  (let loop ((v (slogan tokenizer))
             (exprs '()))
    (if (not (eof-object? v))
        (if (not (void? v)) 
            (loop (slogan tokenizer)
                  (cons v exprs))
            (loop (slogan tokenizer) exprs))
        (reverse exprs))))

(define (compile-slgn-script->scm-script script-name out-file-name
					 assemble exe)
  (call-with-output-file out-file-name
    (lambda (out-port)
      (call-with-input-file (if (not (string-ends-with? script-name *slgn-extn*)) 
				(string-append script-name *slgn-extn*)
				script-name)
	(lambda (port)
	  (let loop ((exprs (compile->scheme (make-tokenizer port compile-mode: (or assemble exe)))))
	    (if (not (null? exprs))
		(begin (write (car exprs) out-port)
		       (newline out-port)
		       (loop (cdr exprs))))))))))

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
                        (string-append *gsc-compiler* " " 
                                       (if cc_options cc_options "")
                                       " -o " (if output output (path-strip-extension script-name))
                                       (if ld_options ld_options "")
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
  (let loop ((len (- (string-length prompt) 2))
             (i 0))
    (if (< len 0)
        (display "> ")
        (if (< i len)
            (begin (display " ")
                   (loop len (+ i 1)))
            (display "> ")))))

(define (braces-matches? s)
  (let ((bcount 0)
        (pcount 0)
        (scount 0))
    (string-foreach 
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
     s)
    (if (< bcount 0) (error "misplaced closing brace."))
    (if (< pcount 0) (error "misplaced closing parenthesis."))
    (if (< scount 0) (error "misplaced closing bracket."))    
    (and (zero? bcount)
         (zero? pcount)
         (zero? scount))))

(define (repl-exception-handler ex)
  (display "error: ")
  (display-exception ex))

(define (repl port #!key (prompt "slogan> "))
  (display prompt) 
  (with-exception-catcher
   repl-exception-handler
   (lambda ()
     (let ((val (let loop ((line (read-line port #\newline #t)))
                  (cond ((and (string-ends-with? (string-rtrim line) ";")
                              (braces-matches? line))
                         (let ((tokenizer (make-tokenizer (open-input-string line))))
                           (let loop ((expr (slogan tokenizer)))
                             (display expr) (newline)
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
                  (newline))))))
  (repl port prompt: prompt))
