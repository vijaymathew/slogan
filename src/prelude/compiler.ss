;; Copyright (c) 2013-2014 by Vijay Mathew Pandyalakal, All Rights Reserved.

(define (compile script-name)
  (with-exception-catcher
   display-exception
   (lambda ()
     (let ((out-file-name (string-append script-name ".scm")))
       (call-with-output-file out-file-name
         (lambda (out-port)
           (write '(load "./prelude/prelude") out-port)
           (call-with-input-file script-name
             (lambda (port)
	       (let ((tokenizer (make-tokenizer port)))
		 (let loop ((v (slogan tokenizer)))
		   (if (not (eof-object? v))
		       (begin (if (not (void? v)) 
				  (begin (write v out-port)
					 (newline out-port)))
			      (loop (slogan tokenizer))))))))))
       (let ((build-cmd (string-append "../platform/gsc/gsc/gsc -o "
				       (string-append script-name ".bin")
				       " -exe "
				       out-file-name)))
	 (if (zero? (shell-command build-cmd))
	     (begin (delete-file out-file-name)
		    #t)
	     #f))))))

(define (import script-name)
  (with-exception-catcher
   display-exception
   (lambda ()
     (call-with-input-file script-name
       (lambda (port)
	 (let ((tokenizer (make-tokenizer port)))
	   (let loop ((v (slogan tokenizer)))
	     (if (not (eof-object? v))
		 (begin (slogan-display (eval v))
			(loop (slogan tokenizer))))))))
     '#!void)))

(define (repl tokenizer #!optional (prompt "slogan> "))
  (display prompt) 
  (if (slogan-display
       (with-exception-catcher 
        display-exception
        (lambda () 
          (let ((expr (slogan tokenizer)))
            (eval expr)))))
      (newline))
  (repl tokenizer prompt))
