(define *scm-extn* ".scm")
(define *slgn-extn* ".sn")
(define *exe-extn* ".run")

(define *prelude-root* (getenv "SLOGAN_PRELUDE_ROOT" "./prelude/"))
(define *load-prelude* `(load (string-append ,*prelude-root* "prelude")))
(define *load-prelude-i* `(load (string-append ,*prelude-root* "prelude.i.scm")))

(define env_prelude_root *prelude-root*)