;; Copyright (c) 2013-2016 by Vijay Mathew Pandyalakal, All Rights Reserved.


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

(define *valid-command-line-options* '("-e" "-c" "-x" "-ld-options" "-cc-options" "-v" "-h" "-r"))

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
  (println "     -r            Launch REPL after performing other options.")
  (println "     -v            Display version information and quit.")
  (println "     -h            Print this help and quit.")
  (println))

(define (show-version)
  (println "slogan version \"" *major-version* "." *minor-version* "-" *release-name* "\"")
  (println))

(define (execute-script scriptname)
  (run-slgn-script scriptname))

(define (compile-script scriptname args exe)
  (let ((ld-options (get-arg-val "-ld-options" args))
        (cc-options (get-arg-val "-cc-options" args)))
    (if exe (compile scriptname exe: #t ld_options: ld-options
                     cc_options: cc-options)
        (compile scriptname assemble: #t ld_options: ld-options
                 cc_options: cc-options))))

(define (command-line-has-options? args)
  (memp (lambda (s) (char=? (string-ref s 0) #\-)) args))

(define (process-args args)
  (assert-command-line-options args)
  (if (has-arg? "-h" args)
      (show-usage))
  (if (has-arg? "-v" args)
      (show-version))
  (if (has-arg? "-e" args)
      (execute-script (get-arg-val "-e" args)))
  (if (has-arg? "-c" args)
      (compile-script (get-arg-val "-c" args) args))
  (if (has-arg? "-x" args)
      (compile-script (get-arg-val "-x" args) args #t))
  (if (command-line-has-options? args)
      (if (has-arg? "-r" args)
          (slogan-repl (current-input-port))
          (exit 0))
      (slogan-repl (current-input-port))))

(process-args (command-line))

