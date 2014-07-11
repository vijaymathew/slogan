;; Copyright (c) 2013-2014 by Vijay Mathew Pandyalakal, All Rights Reserved.


(define (has-arg? arg args)
  (memp (lambda (s) (string=? s arg)) args))

(define (get-arg-val arg args)
  (let loop ((args args))
    (cond ((null? args) #f)
          ((string=? arg (car args))
           (if (null? (cdr args))
               (error "Argument expects a parameter." arg)
               (cadr args)))
          (else (loop (cdr args))))))

(define *valid-command-line-options* '("-e" "-c" "-x" "-ld-options" "-cc-options" "-v" "-h"))

(define (valid-command-line-option? opt)
  (member opt *valid-command-line-options*))

(define (assert-command-line-options args)
  (let loop ((args args))
    (if (null? args) #t
        (begin (if (char=? (string-ref (car args) 0) #\-)
                   (if (not (valid-command-line-option? (car args)))
                       (error "Invalid command line option." (car args))))
               (loop (cdr args))))))

(define (show-usage)
  (println "Usage: slogan [options]")
  (println "             (to start the interactive REPL)")
  (println "or     slogan [options] script")
  (println "             (to execute or compile a slogan script)")
  (println "where options include: ")
  (println "     -e            Execute a script.")
  (println "     -c            Compile a script into a dynamically loadable object file.")
  (println "     -x            Compile a script into an executable binary.")
  (println "     -ld-options   Additional options that will be passed to the system linker.")
  (println "     -cc-options   Additional options that will be passed to the system C compiler.") 
  (println "     -v            Display version information.")
  (println "     -h            Print this help.")
  (println))

(define (show-version)
  (println "slogan version \"" *major-version* "." *minor-version* "-" *release-name* "\"")
  (println))

(define (show-version-and-quit)
  (show-version)
  (exit 0))

(define (show-usage-and-quit)
  (show-usage)
  (exit 0))

(define (execute-script scriptname)
  (run-slgn-script scriptname))

(define (execute-script-and-quit scriptname)
  (execute-script scriptname)
  (exit 0))

(define (compile-script scriptname args exe)
  (let ((ld-options (get-arg-val "-ld-options" args))
        (cc-options (get-arg-val "-cc-options" args)))
    (if exe (compile scriptname exe: #t ld_options: ld-options
                     cc_options: cc-options)
        (compile scriptname assemble: #t ld_options: ld-options
                 cc_options: cc-options))))
  
(define (compile-script-and-quit scriptname args #!optional exe)
  (compile-script scriptname args exe)
  (exit 0))

(define (process-args args)
  (assert-command-line-options args)
  (if (has-arg? "-h" args)
      (show-usage-and-quit))
  (if (has-arg? "-v" args)
      (show-version-and-quit))
  (if (has-arg? "-e" args)
      (execute-script-and-quit (get-arg-val "-e" args)))
  (if (has-arg? "-c" args)
      (compile-script-and-quit (get-arg-val "-c" args) args))
  (if (has-arg? "-x" args)
      (compile-script-and-quit (get-arg-val "-x" args) args #t))
  (repl (current-input-port)))

(process-args (command-line))
