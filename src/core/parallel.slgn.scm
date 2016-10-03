;; Copyright (c) 2013-2016 by Vijay Mathew Pandyalakal, All Rights Reserved.
;; Multi-core programming support.

(define call-fork (c-lambda () int "fork"))

;; Here we do a best effort to find a free port.
(define (next-free-port)
  (let ((port (open-tcp-server (list server-address: "" port-number: 0))))
    (let ((port-number (socket-info-port-number (tcp-server-socket-info port))))
      (close-port port)
      port-number)))

(define-structure process-info id socket server-socket)

(define (parent-process? pinfo)
  (let ((pid (process-info-id pinfo)))
    (if (number? pid)
        (> pid 0)
        #t)))

(define process_id process-info-id)

(define (process-channel pinfo fid)
  (let ((info ((case fid
                 ((self) tcp-client-self-socket-info)
                 ((peer) tcp-client-peer-socket-info)
                 ((remote) tcp-server-socket-info))
               (process-info-socket pinfo))))
    (scm-cons (socket-info-address info)
              (socket-info-port-number info))))

(define (process_self_channel pinfo)
  (process-channel pinfo 'self))

(define (process_peer_channel pinfo)
  (process-channel pinfo 'peer))

(define (process_channel pinfo)
  (process-channel pinfo 'remote))

(define (process_close pinfo)
  (close-port (process-info-socket pinfo))
  (if (parent-process? pinfo)
      (let ((pid (process-info-id pinfo)))
        (close-port (process-info-server-socket pinfo))
        (cond ((number? pid)
               (zero? (scm-kill pid 9)))
              ((thread? pid)
               (thread-terminate! pid)))))
  #t)

(define (invoke-child-callback cb sock)
  (with-exception-catcher
   (lambda (e)
     (close-port sock)
     (scm-println "child process exiting on exception:")
     (show_exception e)
     (scm-exit 1))
   (lambda ()
     (cb (make-process-info 0 sock #f))
     (close-port sock)
     (scm-exit))))

(define (process_connect channel)
  (let ((child (open-tcp-client (list server-address: (scm-car channel)
                                      port-number: (scm-cdr channel)
                                      keep-alive: #t))))
    (let ((ch (make-process-info #f child #f)))
      (if (not (eq? (process_receive ch) 'hi))
          (begin (close-port child)
                 (error "failed to establish connection."))
          ch))))

(define (process child-callback #!optional timeout)
  (let ((port-number (next-free-port)))
    (let ((pid (call-fork)))
      (cond ((zero? pid)
             (let ((sock (open-tcp-client (list port-number: port-number
                                                keep-alive: #t))))
               (invoke-child-callback child-callback sock)))
            ((> pid 0)
             (let ((sock (open-tcp-server (list port-number: port-number
                                                coalesce: #f))))
               (if timeout
                   (input-port-timeout-set! sock timeout))
               (let ((conn (scm-read sock)))
                 (if (eof-object? conn)
                     (error "timedout waiting for child process.")
                     (make-process-info pid conn sock)))))
            (else #f)))))

(define (process_send pinfo object #!optional timeout)
  (let ((out (process-info-socket pinfo)))
    (if timeout
        (if (scm-not (> timeout 0))
            (error "process-send - timeout must be a positive number.")
            (output-port-timeout-set! out timeout)))
    (scm-showln stream: out quotes: #t object)
    (force-output out)))

(define (normalize-msg msg)
  (if (and (pair? msg) (eq? (scm-car msg) 'quote))
      (scm-cadr msg)
      msg))

(define (process_receive pinfo #!optional timeout default)
  (let ((in (process-info-socket pinfo)))
    (if timeout
        (if (scm-not (> timeout 0))
            (error "process-recv - timeout must be a positive number.")
            (input-port-timeout-set! in timeout)))
    (let ((r (read-line in)))
      (cond ((eof-object? r)
             (if timeout default r))
            (else
             (let ((buf (open-input-string r)))
               (let ((t (make-tokenizer buf '())))
                 (let ((r (normalize-msg (scm-slogan t))))
                   (close-input-port buf)
                   r))))))))

(define *eval-prefixes* (scm-append '(scm-cons scm-list scm-long-list make-equal-hashtable)
                                    *vector-patterns*)) ;; see match.slgn.scm

(define (needs-eval? v)
  (if (pair? v)
      (scm-member (scm-car v) *eval-prefixes*)
      #f))

(define (spawn child-callback #!optional timeout default)
  (define (cb pinfo)
    (let ((message (process_receive pinfo timeout default)))
      (cond ((not (eq? message 'quit))
             (process_send pinfo (child-callback message) timeout)
             (cb pinfo)))))
  (let ((pinfo (process cb timeout)))
    (lambda (message)
      (cond ((eq? message 'info)
             pinfo)
            ((eq? message 'quit)
             (process_send pinfo message timeout)
             (process_close pinfo))
            (else
             (process_send pinfo message timeout)
             (let ((value *void*))
               (lambda (#!key timeout default)
                 (if (scm-eq? value *void*)
                     (set! value (let ((v (process_receive pinfo timeout default)))
                                   (if (needs-eval? v)
                                       (scm-eval v)
                                       v))))
                 value)))))))

