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

(define compile
  (lambda (script-name #!key 
		       (assemble #f)
		       (exe #f))      
    (with-exception-catcher
     display-exception
     (lambda ()
       (let ((out-file-name (string-append script-name *scm-extn*)))
	 (call-with-output-file out-file-name
	   (lambda (out-port)
	     (call-with-input-file (if (not (string-endswith? script-name *slgn-extn*)) 
				       (string-append script-name *slgn-extn*)
				       script-name)
	       (lambda (port)
		 (let loop ((exprs (compile->scheme (make-tokenizer port compile-mode: (or assemble exe)))))
		   (if (not (null? exprs))
		       (begin (write (car exprs) out-port)
			      (newline out-port)
			      (loop (cdr exprs)))))))))
	 (if (or assemble exe)
	     (let ((build-cmd (if exe 
				  (string-append "../platform/gsc/gsc/gsc -o "
						 (string-append script-name *exe-extn*)
						 " -exe "
						 (string-append *prelude-root* "/*.scm ")
						 out-file-name)
				  (string-append "../platform/gsc/gsc/gsc " out-file-name))))
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

(define (repl port #!optional (prompt "slogan> "))
  (display prompt) 
  (if (slogan-display
       (with-exception-catcher 
        display-exception
        (lambda ()
          (let loop ((line (read-line port)))
            (cond ((string_ends_with? line ";")
                   (let ((expr (slogan (make-tokenizer (open-input-string line)))))
                     (eval expr)))
                  (else (show-waiting-prompt prompt)
                        (loop (string-append line (read-line port)))))))))
      (newline))
  (repl port prompt))
