(define *scm-extn* ".scm")
(define *slgn-extn* ".sn")
(define *exe-extn* ".run")

(define *prelude-root* (getenv "SLOGAN_PRELUDE_ROOT"))
(if (not *prelude-root*)
    (begin (error "The environment variable SLOGAN_PRELUDE_ROOT is not set.")
           (exit 1)))
                     
(define *load-prelude* `(load (string-append ,*prelude-root* "/prelude/prelude")))
(define *load-prelude-i* `(load (string-append ,*prelude-root* "/prelude/prelude.i.scm")))
