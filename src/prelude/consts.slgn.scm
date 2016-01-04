(define *major-version* 0)
(define *minor-version* 1)
(define *release-name* 'beta)

(define *scm-extn* ".scm")
(define *slgn-extn* ".sn")
(define *exe-extn* ".run")
(define *obj-extn* ".o1")

(define *path-sep* "/")
(define *slogan-config-dir* "/etc/slogan")

(define (read-slogan-root)
  (let ((root-file (string-append *slogan-config-dir* *path-sep* "root")))
    (cond ((file-exists? root-file)
           (call-with-input-file root-file
             (lambda (p)
               (read-line p))))
          (else
           (getenv "SLOGAN_ROOT" ".")))))

(define *slogan-root* (read-slogan-root))
(define *prelude-root* (string-append *slogan-root* "/src/prelude"))
(define *pkg-root* (string-append *slogan-root* "/packages/"))

(define (slogan_root) *slogan-root*)

(define *gsc-compiler* (string-append *slogan-root* "/platform/gsc/gsc/gsc"))

(define *libffi-path* (string-append *slogan-root* "/platform/libffi-3.2.1"))
(define *libffi-lib* (string-append *libffi-path* "/target/usr/local/lib"))
(define *libffi-inc* (string-append *libffi-path* "/target/usr/local/lib/libffi-3.2.1/include"))
(define *default-cc-options* (string-append "-I" *libffi-inc*))
(define *default-ld-options* (string-append "-L" *libffi-lib* " -lffi"))

(define *default-eq* eqv?)

(define *slogan-reprs* '((true . #t) (false . #f)))                         
(define *scheme-reprs* '((#t . true) (#f . false)))

(define *void* '#!void)
