(define *scm-extn* ".scm")
(define *slgn-extn* ".sn")
(define *exe-extn* ".run")

(define *slogan-root* (getenv "SLOGAN_ROOT" "."))
(define *prelude-root* (string-append *slogan-root* "/src/prelude"))

(define *gsc-compiler* (string-append *slogan-root* "/platform/gsc/gsc/gsc"))
(define *platform-incs* (string-append *slogan-root* "/platform/nanomsg/src"))
(define *platform-libs* (string-append *slogan-root* "/platform/nanomsg/.libs"))
(define *cc-options* (string-append "-cc-options \"-I" *platform-incs* "\""))
(define *ld-options* (string-append "-ld-options \"-L" *platform-libs* " -lpthread -lnanomsg -lanl\""))
