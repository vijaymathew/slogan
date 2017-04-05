;; Copyright (c) 2013-2017 by Vijay Mathew Pandyalakal, All Rights Reserved.

(define (has-arg? arg args)
  (scm-memp (lambda (s) (string=? s arg)) args))

(define (get-arg-val arg args)
  (let loop ((args args))
    (cond ((null? args) #f)
          ((string=? arg (scm-car args))
           (if (null? (scm-cdr args))
               (scm-error "missing parameter" arg)
               (scm-cadr args)))
          (else (loop (scm-cdr args))))))

(define *valid-command-line-options* '("-e" "-c" "-x" "-ld-options" "-cc-options" "-v" "-h" "-r" "-i" "-u"))

(define (valid-command-line-option? opt)
  (scm-member opt *valid-command-line-options*))

(define (assert-command-line-options args)
  (let loop ((args args))
    (if (null? args) #t
        (begin (if (char=? (string-ref (scm-car args) 0) #\-)
                   (if (scm-not (valid-command-line-option? (scm-car args)))
                       (scm-error "invalid command line option" (scm-car args))))
               (loop (scm-cdr args))))))

(define (show-vm-usage)
  (let ((msg "Usage: program [-:OPTION,OPTION...] ...
where OPTION is one of:
     mHEAPSIZE       set minimum heap size in kilobytes
     hHEAPSIZE       set maximum heap size in kilobytes
     lLIVEPERCENT    set heap live ratio after GC in percent
     s|S             set standard Scheme mode (on|off)
     d[OPT...]       set debugging options; OPT is one of:
                         p|a       treat uncaught exceptions as errors
                                   (primordial-thread only|all threads)
                         r|s|q     error handling (create a new REPL|start in
                                   single-step mode|quit with error status)
                         R|D|Q     user interrupt handling (create a new REPL|
                                   defer handling|quit with error status)
                         i|c|-|@[HOST][:PORT]
                                   select REPL interaction channel (ide|console|
                                   standard input and output|remote debugger
                                   (defaults: HOST=127.0.0.1, PORT=44555))
                         0..9      verbosity level
     @[INTF][:PORT]  set main RPC server configuration; defaults: INTF=127.0.0.1,
                     PORT=44556; when INTF=* all interfaces accept connections
     =DIRECTORY      override central Gambit installation directory
     ~~DIR=DIRECTORY override Gambit installation directory ~~DIR (where DIR can
                     be the special \"bin\" and \"lib\", or empty, or any identifier)
     +ARGUMENT       add ARGUMENT to the command line before other arguments
     f[OPT...]       set file options; see below for OPT
     t[OPT...]       set terminal options; see below for OPT
     -[OPT...]       set standard input and output options; see below for OPT
                     where OPT is one of:
                         A|1|2|4|6|8|U   character encoding (ASCII|ISO-8859-1|UCS-2/4|UTF-16/8|UTF)
                         l|c|cl          end-of-line encoding (LF|CR|CR-LF)
                         u|n|f           buffering (unbuffered|newline buffered|fully buffered)
                         r|R             enable character encoding errors (on|off)
                         e|E             [for terminals only] enable line-editing (on|off)"))
    (scm-newline)
    (scm-display "The options accepted by the Gambit VM:")
    (scm-newline)
    (scm-display msg)
    (scm-newline)))
    
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
  (scm-println)
  (show-vm-usage))   

(define (show-version)
  (scm-println "slogan version \"" *major-version* "." *minor-version* "-" *release-name* "\"")
  (scm-println))

(define (execute-script scriptname)
  (run-slgn-script scriptname))

(define (compile-script scriptname args exe)
  (let ((ld-options (get-arg-val "-ld-options" args))
        (cc-options (get-arg-val "-cc-options" args)))
    (if exe (slgn-compile scriptname exe: #t ld_options: ld-options
                          cc_options: cc-options)
        (slgn-compile scriptname assemble: #t ld_options: ld-options
                      cc_options: cc-options))))

(define (install-pkg args)
  (if (scm-not (scm->= (length args) 3))
      (scm-error "-i \"package-name,type,location\"")
      (install_package (scm-nth 0 args) (string->symbol (scm-nth 1 args))
                       (scm-nth 2 args) (if (scm-> (scm-length args) 3)
                                            (scm-= (scm-nth 2 args) "true")
                                            #f))))

(define (uninstall-pkg name)
  (uninstall_package name))

(define (command-line-has-options? args)
  (scm-memp (lambda (s) (char=? (string-ref s 0) #\-)) args))

(define *user-args* '())

(define (filter-command-line-options args)
  (let recur ((args args))
    (if (null? args)
        '()
        (let ((a (scm-car args)))
          (if (valid-command-line-option? a)
              (if (scm-not (or (string=? a "-h")
                           (string=? a "-r")
                           (string=? a "-v")))
                  (recur (scm-cddr args))
                  (recur (scm-cdr args)))
              (scm-cons a (recur (scm-cdr args))))))))

(define (process-args args)
  (assert-command-line-options args)
  (set! *user-args* (filter-command-line-options args))
  (if (has-arg? "-h" args)
      (show-usage))
  (if (has-arg? "-v" args)
      (show-version))
  (if (has-arg? "-e" args)
      (execute-script (get-arg-val "-e" args)))
  (if (has-arg? "-c" args)
      (compile-script (get-arg-val "-c" args) args #f))
  (if (has-arg? "-i" args)
      (install-pkg (string-split (get-arg-val "-i" args) #\,)))
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

