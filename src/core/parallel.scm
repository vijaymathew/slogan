;; Copyright (c) 2013-2018 by Vijay Mathew Pandyalakal, All Rights Reserved.
;; Multi-core programming support.

(c-declare #<<c-declare-end

#include <sys/wait.h>
#include <unistd.h>
#include <sched.h>

static int call_waitpid(pid_t pid)
{
 /* Make sure the child process does not become a zombie.
    Should be redundant for Linux 2.6+, where the disposition
    of SIGCHLD is set to SIG_IGN by default, and such process does
    not become zombies. */
  int status;
  pid_t r = waitpid(pid, &status, 0);
  if (r == pid)
    if (WIFEXITED(status)) return 1;
    else return 2;
  else if (r == 0) return 0;
  else if (r < 0) return -1;
}
c-declare-end
)

(define call-fork (c-lambda () int "fork"))
(define call-waitpid (c-lambda (int) int "call_waitpid"))

(define proc-port-min 10000)
(define proc-port-max 65535)
(define proc-max-retries 10)
(define proc-port-curr proc-port-min)
(define channel-mutex (make-mutex))

(define-structure process-info pid channel1 channel2 server-channels)

(define (set_process_base_port pmin)
  (if (and (scm->= pmin 1024)
           (scm-<= pmin proc-port-max))
      (begin
        (set! proc-port-min pmin)
        (set! proc-port-curr pmin))
      (scm-error "port out-of-range")))

(define (create-process-channel #!optional (retries 0))
  (mutex-lock! channel-mutex)
  (with-exception-catcher
   (lambda (e)
     (if (scm-> retries proc-max-retries)
         (begin (mutex-unlock! channel-mutex)
                (scm-raise e))
         (if (scm-> proc-port-curr proc-port-max)
             (begin (set! proc-port-curr proc-port-min)
                    (mutex-unlock! channel-mutex)
                    (scm-raise e))
             (begin (set! proc-port-curr (scm-+ proc-port-curr 1))
                    (mutex-unlock! channel-mutex)
                    (create-process-channel (scm-+ retries 1))))))
   (lambda ()
     (if (scm-> proc-port-curr proc-port-max)
         (set! proc-port-curr proc-port-min))
     (let ((channel (open-tcp-server (scm-list server-address: "" ; accept connections only on the loopback interface.
                                               reuse-address: #f
                                               port-number: proc-port-curr)))
           (port proc-port-curr))
       (set! proc-port-curr (scm-+ proc-port-curr 1))
       (let ((r (scm-cons channel port)))
         (mutex-unlock! channel-mutex)
         r)))))

(define proc-channel-stream scm-car)
(define proc-channel-port scm-cdr)

(define (process-out-channel pinfo)
  (if (zero? (process-info-pid pinfo))
      (process-info-channel2 pinfo)
      (process-info-channel1 pinfo)))

(define (process-in-channel pinfo)
  (if (zero? (process-info-pid pinfo))
      (process-info-channel1 pinfo)
      (process-info-channel2 pinfo)))

(define (scm-process_close pinfo)
  (with-exception-catcher
   (lambda (e)
     #f)
   (lambda ()
     (close-port (process-in-channel pinfo))
     (close-port (process-out-channel pinfo))
     (let ((pid (process-info-pid pinfo)))
       (if (scm-not (zero? pid))
	   (let ((server-channels (process-info-server-channels pinfo)))
	     (close-port (proc-channel-stream (scm-car server-channels)))
	     (close-port (proc-channel-stream (scm-cdr server-channels)))
	     (call-waitpid pid))))
     #t)))

(define (invoke-child-callback callback pinfo)
  (with-exception-catcher
   (lambda (e)
     (scm-process_close pinfo)
     (scm-exit 1))
   (lambda ()
     (callback pinfo)
     (scm-process_close pinfo)
     (scm-exit))))

(define (make-proc-io-channels)
  (scm-cons (create-process-channel) (create-process-channel)))

(define (connect-server-proc channel)
  (open-tcp-client (string-append "localhost:" (number->string (proc-channel-port channel)))))

(define (wait-for-process-channels in-channel out-channel)
  (scm-cons (connect-server-proc in-channel)
            (connect-server-proc out-channel)))

(define (scm-process child-callback)
  (let ((in-out (make-proc-io-channels))
        (pid (call-fork)))
    (cond ((zero? pid)
           (let ((io-streams (wait-for-process-channels (scm-car in-out) (scm-cdr in-out))))
             (invoke-child-callback
              child-callback (make-process-info
                              0 (scm-car io-streams)
                              (scm-cdr io-streams) #f))))
          ((scm-> pid 0)
           (make-process-info pid (scm-read (proc-channel-stream (scm-car in-out)))
                              (scm-read (proc-channel-stream (scm-cdr in-out)))
                              in-out))
          (else (scm-error "failed to spawn child process.")))))

(define (scm-process_send pinfo object)
  (let ((out (process-out-channel pinfo)))
    (let ((str-out (open-output-string)))
      (scm-show stream: str-out quotes: #t object)
      (force-output str-out)
      (let ((str (get-output-string str-out)))
        (scm-write str out)
        (force-output out)))))

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

(define (proc-recv in)
  (eval-message
   (let ((r (scm-read in)))
     (cond ((eof-object? r)
            r)
           (else
            (let ((buf (open-input-string r)))
              (let ((t (make-tokenizer buf '())))
                (normalize-msg (scm-slogan t)))))))))

(define (scm-process_receive pinfo #!optional timeout default)
  (let ((in (process-in-channel pinfo)))
    (cond
     ((scm-not timeout)
      (input-port-timeout-set! in +inf.0)
      (proc-recv in))
     (else
      (input-port-timeout-set! in timeout)
      (let ((r (proc-recv in)))
        (if (eof-object? r)
            default
            r))))))

(define process scm-process)
(define process_send scm-process_send)
(define process_receive scm-process_receive)
(define process_close scm-process_close)
