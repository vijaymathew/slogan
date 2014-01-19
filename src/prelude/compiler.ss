;; Copyright (c) 2013-2014 by Vijay Mathew Pandyalakal, All Rights Reserved.

(define (compile-line port)
  (expression/statement (make-tokenizer port)))

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
               (let loop ((v (compile-line port)))
                 (if (not (eof-object? v))
                     (begin (if (not (void? v)) 
				(begin (write v out-port)
				       (newline out-port)))
                            (loop (program port)))))))))
       (let ((build-cmd (string-append "../platform/gsc/gsc/gsc -o "
                                       (string-append script-name ".bin")
                                       " -exe "
                                       out-file-name)))
         (zero? (shell-command build-cmd)))))))

(define (import script-name)
  (with-exception-catcher
   display-exception
   (lambda ()
     (call-with-input-file script-name
       (lambda (port)
         (let loop ((v (compile-line port)))
           (if (not (eof-object? v))
               (begin (slogan-display (eval v))
                      (loop (program port)))))))
     '#!void)))

(define (repl #!optional (port (current-input-port)) 
	      (prompt "slogan> "))
  (display prompt) 
  (if (slogan-display
       (with-exception-catcher 
        display-exception
        (lambda () 
          (let ((expr (compile-line port)))
	    (display expr) (newline)
            (eval expr)))))
      (newline))
  (repl port prompt))
