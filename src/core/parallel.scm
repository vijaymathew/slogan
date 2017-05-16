;; Copyright (c) 2013-2017 by Vijay Mathew Pandyalakal, All Rights Reserved.
;; Multi-core programming support.

(c-declare #<<c-declare-end

#include <unistd.h>
#include <sched.h>

c-declare-end
)

(define call-fork (c-lambda () int "fork"))

(define process-info scm-cons)
(define process-id scm-car)
(define process-child-pid scm-cdr)

(define (process-channel pinfo)
  (let ((s (open-output-string)))
    (scm-display ".proc." s)
    (scm-display (process-id pinfo) s)
    (get-output-string s)))

(define (process-channel-out channel)
  (string-append channel "/out"))

(define (safe-directory-files dir)
  (with-exception-catcher
   (lambda (_) '())
   (lambda ()
     (if (file-exists? dir)
         (directory-files dir)
         '()))))

(define (next-channel-entry dir)
  (string-append dir "/r." (number->string (scm-length (safe-directory-files dir)))))

(define (process-out-channel pinfo)
  (let ((child-pid (process-child-pid pinfo))
        (channel (process-channel pinfo)))
    (if (zero? child-pid)
        channel
        (process-channel-out channel))))

(define (process-in-channel pinfo)
  (let ((child-pid (process-child-pid pinfo))
        (channel (process-channel pinfo)))
    (if (zero? child-pid)
        (process-channel-out channel)
        channel)))

(define (create-channel! pinfo)
  (let ((channel (process-channel pinfo)))
    (if (scm-not (file-exists? channel))
        (create-directory channel))
    (let ((out (process-channel-out channel)))
      (if (scm-not (file-exists? out))
          (create-directory out)))))

(define (wait-for-process-channel pinfo)
  (let ((channel (process-channel pinfo)))
    (if (scm-not (file-exists? channel))
        (begin (thread-sleep! .2)
               (wait-for-process-channel pinfo)))
    (let ((out (process-channel-out channel)))
      (if (scm-not (file-exists? out))
          (begin (thread-sleep! .2)
                 (wait-for-process-channel pinfo))))))

(define (safe-delete-proc-channel-obj file delfn)
  (with-exception-catcher
   (lambda (_) #f)
   (lambda ()
     (if (file-exists? file)
         (delfn file)))))

(define (safe-delete-proc-dir file)
  (safe-delete-proc-channel-obj file delete-directory))

(define (safe-delete-proc-file file)
  (safe-delete-proc-channel-obj file delete-file))

(define (cleanup-channel-files channel)
  (let loop ((files (safe-directory-files (scm-list path: channel ignore-hidden: #f))))
    (if (scm-not (null? files))
        (begin (safe-delete-proc-file (string-append channel "/" (scm-car files)))
               (loop (scm-cdr files))))))

(define (remove-dir! channel)
  (cleanup-channel-files channel)
  (safe-delete-proc-dir channel))

(define (remove-channel! pinfo)
  (let ((channel (process-channel pinfo)))
    (remove-dir! (process-channel-out channel))
    (remove-dir! channel)))

(define (scm-process_close pinfo)
  (let ((child-pid (process-child-pid pinfo)))
    (if (> child-pid 0)
        (begin (scm-kill child-pid 9)
               (remove-channel! pinfo))))
  #t)

(define pid-counter 0)
(define pid-counter-lock (make-mutex))

(define (next-process-id)
  (mutex-lock! pid-counter-lock)
  (let ((pid (time->seconds (current-time))))
    (cond ((equal? pid pid-counter)
           (mutex-unlock! pid-counter-lock)
           (next-process-id))
          (else
           (set! pid-counter pid)
           (mutex-unlock! pid-counter-lock)
           pid))))

(define (invoke-child-callback callback pinfo)
  (with-exception-catcher
   (lambda (e)
     (scm-println "child process exiting on exception:")
     (scm-show_exception e)
     (scm-exit 1))
   (lambda ()
     (callback pinfo)
     (scm-exit))))

(define (scm-process child-callback)
  (let ((id (next-process-id))
        (child-pid (call-fork)))
    (cond ((zero? child-pid)
           (let ((pinfo (process-info id child-pid)))
             (wait-for-process-channel pinfo)
             (invoke-child-callback child-callback pinfo)))
          ((scm-> child-pid 0)
           (let ((pinfo (process-info id child-pid)))
             (create-channel! pinfo)
             pinfo))
          (else (scm-error "failed to spawn child process.")))))

(define (scm-process_send pinfo object)
  (let ((out (process-out-channel pinfo)))
    (if (scm-not (file-exists? out))
        (scm-error "invalid or closed process output channel"))
    (call-with-output-file
        (next-channel-entry out)
      (lambda (out)
        (scm-show stream: out quotes: #t object)))))

(define (normalize-msg msg)
  (if (and (pair? msg) (eq? (scm-car msg) 'quote))
      (scm-cadr msg)
      msg))

(define *eval-prefixes* (scm-append '(scm-cons scm-list scm-long-list make-equal-hashtable)
                                    *vector-patterns*)) ;; see match.slgn.scm

(define (needs-eval? v)
  (if (pair? v)
      (scm-member (scm-car v) *eval-prefixes*)
      #f))

(define (eval-message m)
  (if (needs-eval? m)
      (scm-eval m)
      m))

(define (process-recv file)
  (if (scm-not (file-exists? file))
      (scm-error "invalid or closed process input channel"))
  (let ((r
         (call-with-input-file file
           (lambda (in)
             (let ((r (scm-read_all_chars in)))
               (cond ((eof-object? r)
                      r)
                     (else
                      (let ((buf (open-input-string r)))
                        (let ((t (make-tokenizer buf '())))
                          (normalize-msg (scm-slogan t)))))))))))
    (safe-delete-proc-file file)
    (eval-message r)))

(define (scm-process_receive pinfo #!optional timeout default)
  (let ((indir (process-in-channel pinfo)))
    (let loop ((files (safe-directory-files indir))
               (timeout timeout))
      (if (null? files)
          (if timeout
              (cond ((scm-> timeout 0)
                     (thread-yield!)
                     (loop (safe-directory-files indir)
                           (scm-- timeout .1)))
                    (else default))
              (begin
                (thread-yield!)
                (loop (safe-directory-files indir) timeout)))
          (if (string=? (scm-car files) "out")
              (loop (scm-cdr files) timeout)
              (process-recv (string-append indir "/" (scm-car files))))))))

(define process scm-process)
(define process_send scm-process_send)
(define process_receive scm-process_receive)
(define process_close scm-process_close)
