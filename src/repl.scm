;; Copyright (c) 2013-2016 by Vijay Mathew Pandyalakal, All Rights Reserved.


(define (has-arg? arg args)
  (scm-memp (lambda (s) (string=? s arg)) args))

(define (get-arg-val arg args)
  (let loop ((args args))
    (cond ((null? args) #f)
          ((string=? arg (scm-car args))
           (if (null? (scm-cdr args))
               (error "Argument expects a parameter." arg)
               (scm-cadr args)))
          (else (loop (scm-cdr args))))))

(define *valid-command-line-options* '("-e" "-c" "-x" "-ld-options" "-cc-options" "-v" "-h" "-r" "-i" "-u"))

(define (valid-command-line-option? opt)
  (member opt *valid-command-line-options*))

(define (assert-command-line-options args)
  (let loop ((args args))
    (if (null? args) #t
        (begin (if (char=? (string-ref (scm-car args) 0) #\-)
                   (if (not (valid-command-line-option? (scm-car args)))
                       (error "Invalid command line option." (scm-car args))))
               (loop (scm-cdr args))))))

(define (show-usage)
  (scm-println "Usage: slogan [options]")
  (scm-println "             (to start the interactive REPL)")
  (scm-println "or     slogan [options] script")
  (scm-println "             (to execute or compile a slogan script)")
  (scm-println "where options include: ")
  (scm-println "     -e            Execute a script.")
  (scm-println "     -c            Compile a script into a dynamically loadable object file.")
  (scm-println "     -x            Compile a script into an executable binary.")
  (scm-println "     -i            Install a package.")
  (scm-println "     -u            Uninstall a package.")
  (scm-println "     -ld-options   Additional options that will be passed to the system linker.")
  (scm-println "     -cc-options   Additional options that will be passed to the system C compiler.") 
  (scm-println "     -r            Launch REPL after performing other options.")
  (scm-println "     -v            Display version information and quit.")
  (scm-println "     -h            Print this help and quit.")
  (scm-println))

(define (show-version)
  (scm-println "slogan version \"" *major-version* "." *minor-version* "-" *release-name* "\"")
  (scm-println))

(define (execute-script scriptname)
  (run-slgn-script scriptname))

(define (compile-script scriptname args exe)
  (let ((ld-options (get-arg-val "-ld-options" args))
        (cc-options (get-arg-val "-cc-options" args)))
    (if exe (compile scriptname exe: #t ld_options: ld-options
                     cc_options: cc-options)
        (compile scriptname assemble: #t ld_options: ld-options
                 cc_options: cc-options))))

(define (install-pkg args)
  (if (not (>= (length args) 3))
      (error "-i \"package-name,type,location\"")
      (install_package (nth 0 args) (string->symbol (nth 1 args))
                       (nth 2 args) (if (> (length args) 3)
                                        (= (nth 2 args) "true")
                                        #f))))

(define (uninstall-pkg name)
  (uninstall_package name))

(define (command-line-has-options? args)
  (scm-memp (lambda (s) (char=? (string-ref s 0) #\-)) args))

(define (process-args args)
  (assert-command-line-options args)
  (if (has-arg? "-h" args)
      (show-usage))
  (if (has-arg? "-v" args)
      (show-version))
  (if (has-arg? "-e" args)
      (execute-script (get-arg-val "-e" args)))
  (if (has-arg? "-c" args)
      (compile-script (get-arg-val "-c" args) args #f))
  (if (has-arg? "-i" args)
      (install-pkg (string_split (get-arg-val "-i" args) #\,)))
  (if (has-arg? "-u" args)
      (uninstall-pkg (get-arg-val "-u" args)))
  (if (has-arg? "-x" args)
      (compile-script (get-arg-val "-x" args) args #t))
  (if (command-line-has-options? args)
      (if (has-arg? "-r" args)
          (slogan-repl (current-input-port))
          (exit 0))
      (slogan-repl (current-input-port))))

(process-args (command-line))

