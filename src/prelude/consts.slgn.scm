(define *major-version* 0)
(define *minor-version* 1)
(define *release-name* 'beta)

(define *scm-extn* ".scm")
(define *slgn-extn* ".sn")
(define *exe-extn* ".run")

(define *slogan-root* (getenv "SLOGAN_ROOT" "."))
(define *prelude-root* (string-append *slogan-root* "/src/prelude"))

(define *gsc-compiler* (string-append *slogan-root* "/platform/gsc/gsc/gsc"))

(define *default-eq* eqv?)

(define *slogan-reprs* '((true . #t) (false . #f)))                         
(define *scheme-reprs* '((#t . true) (#f . false)))

(define *void* '#!void)
