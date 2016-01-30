;; Copyright (c) 2013-2016 by Vijay Mathew Pandyalakal, All Rights Reserved.
;; Multi-core programming support.


(define call-fork (c-lambda () int "fork"))

;; Here we do a best effort to find a free port.
(define (next-free-port)
  (let ((port (open-tcp-server (list server-address: "" port-number: 0))))
    (let ((port-number (socket-info-port-number (tcp-server-socket-info port))))
      (close-port port)
      port-number)))

(define-structure process-info id socket)

(define (parent-process? pinfo)
  (let ((pid (process-info-id pinfo)))
    (if (number? pid)
        (> pid 0)
        #t)))

(define process_id process-info-id)

(define (process_close pinfo)
  (close-port (process-info-socket pinfo))
  (if (and (parent-process? pinfo)
           (number? (process-info-id pinfo)))
      (scm-kill (process-info-id pinfo) 9))
  #t)

(define (invoke-child-callback cb sock)
  (with-exception-catcher
   (lambda (e)
     (close-port sock)
     (scm-println "exiting after exception - " e)
     (scm-exit 1))
   (lambda ()
     (cb (make-process-info 0 sock))
     (close-port sock)
     (scm-exit))))

(define (process-connect address)
  (let ((child (open-tcp-client (list server-address: address
                                      keep-alive: #t))))
    (make-process-info #f child)))

(define (process child-callback/address #!optional timeout)
  (if (string? child-callback/address)
      (process-connect child-callback/address)
      (let ((port-number (next-free-port)))
        (let ((pid (call-fork)))
          (cond ((zero? pid)
                 (let ((sock (open-tcp-client port-number)))
                   (invoke-child-callback child-callback/address sock)))
                ((> pid 0)
                 (let ((sock (open-tcp-server port-number)))
                   (if timeout
                       (input-port-timeout-set! sock timeout))
                   (let ((conn (scm-read sock)))
                     (if (eof-object? conn)
                         (error "timedout waiting for child process.")
                         (make-process-info pid conn)))))
                (else #f))))))

(define (process_send pinfo object #!optional timeout)
  (let ((out (process-info-socket pinfo)))
    (if timeout
        (if (scm-not (> timeout 0))
            (error "process-send - timeout must be a positive number.")
            (output-port-timeout-set! out timeout)))
    (scm-write object out)
    (newline out)
    (force-output out)))

(define (process_receive pinfo #!optional timeout default)
  (let ((in (process-info-socket pinfo)))
    (if timeout
        (if (scm-not (> timeout 0))
            (error "process-recv - timeout must be a positive number.")
            (input-port-timeout-set! in timeout)))
    (let ((r (read-line in)))
      (if (and timeout (eof-object? r))
          default
          (let ((buf (open-input-string r)))
            (let ((r (scm-read buf)))
              (close-input-port buf)
              r))))))
         
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
                     (set! value (process_receive pinfo timeout default)))
                 value)))))))

(define (process_terminate pinfo)
  (if (number? (process-info-id pinfo))
      (zero? (scm-kill (process-info-id pinfo) 9))
      #f))
