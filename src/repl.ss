(load "prelude/prelude")
(load "util")
(load "tokenizer")
(load "parser")

(define (import script-name)
  (with-exception-catcher
   display-exception
   (lambda ()
     (call-with-input-file script-name
       (lambda (port)
         (let loop ((v (program port)))
           (if (not (eof-object? v))
               (begin (slogan-display (eval v))
                      (loop (program port)))))))
     '#!void)))

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
               (let loop ((v (program port)))
                 (if (not (eof-object? v))
                     (begin (write v out-port)
                            (loop (program port)))))))))
       (let ((build-cmd (string-append "../platform/gsc/gsc/gsc -o "
                                       (string-append script-name ".bin")
                                       " -exe "
                                       out-file-name)))
         (zero? (shell-command build-cmd)))))))

(define (repl)
  (display "slogan> ") 
  (if (slogan-display
       (with-exception-catcher 
        display-exception
        (lambda () 
          (let ((expr (program (current-input-port))))
            (eval expr)))))
      (newline))
  (repl))

(repl)