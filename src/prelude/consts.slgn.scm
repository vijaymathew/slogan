(define *scm-extn* ".scm")
(define *slgn-extn* ".sn")
(define *exe-extn* ".run")

(define *slogan-root* (getenv "SLOGAN_ROOT" "/usr/slogan"))
(define *prelude-root* (string-append *slogan-root* "/src/prelude"))

(define *gsc-compiler* (string-append *slogan-root* "/platform/gsc/gsc/gsc"))

(define *default-eq* eqv?)

(define *slogan-reprs* '((true . #t) (false . #f)))                         
(define *scheme-reprs* '((#t . true) (#f . false)))

(define *void* '#!void)

(define *pos-inf-sym* '|+inf.0|)
(define *neg-inf-sym* '|-inf.0|)
