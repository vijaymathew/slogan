;; Copyright (c) 2013-2016 by Vijay Mathew Pandyalakal, All Rights Reserved.
;; Multi-core programming support.

(c-declare #<<c-declare-end
           
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <fcntl.h>
#include "../include/slogan.h"

#define C_RECV_BUFSZ 1024
 
 static void set_nonblocking(int fd)
 {
   int flags = fcntl(fd, F_GETFL, 0);
   if (fcntl(fd, F_SETFL, flags | O_NONBLOCK) == -1)
     fprintf(stderr, "set_nonblocking - fcntl failed for %d - %d. %s\n", fd, errno, strerror(errno));
 }
 
 static int c_server_socket(char *path, int backlog)
 {
   int s;
   struct sockaddr_un addr;
   
   if ((s = socket(AF_UNIX, SOCK_STREAM, 0)) == -1)
     {
       fprintf(stderr, "socket failed - %d. %s\n", errno, strerror(errno));
       return -1;
     }

   memset(&addr, 0, sizeof(addr));
   addr.sun_family = AF_UNIX;
   strncpy(addr.sun_path, path, sizeof(addr.sun_path)-1);
   unlink(addr.sun_path);

   if (bind(s, (struct sockaddr *)&addr, sizeof(addr)) == -1)
     {
       fprintf(stderr, "bind failed - %d. %s\n", errno, strerror(errno));
       return -2;
     }
    
   if (listen(s, backlog) == -1)
     {
       fprintf(stderr, "listen failed - %d. %s\n", errno, strerror(errno));
       return -3;
     }
   set_nonblocking(s);
   return s;
 }

 static int c_client_socket(char *path)
 {
   int s;
   struct sockaddr_un addr;

   if ((s = socket(AF_UNIX, SOCK_STREAM, 0)) == -1)
     {
       fprintf(stderr, "client-socket failed - %d. %s\n", errno, strerror(errno));
       return -1;
     }

   memset(&addr, 0, sizeof(addr));
   addr.sun_family = AF_UNIX;
   strncpy(addr.sun_path, path, sizeof(addr.sun_path)-1);
   if (connect(s, (struct sockaddr *)&addr, sizeof(addr)) == -1)
     {
       fprintf(stderr, "connect failed - %d. %s\n", errno, strerror(errno));
       return -2;
     }
   set_nonblocking(s);
   return s;
 }
 
 static int c_accept(int s)
 {
   struct sockaddr_un addr;
   int s2;
   int t = sizeof(addr);
   
   s2 = accept(s, (struct sockaddr *)&addr, &t);
   if (s2 == -1)
     {
       if (errno == EAGAIN || errno == EWOULDBLOCK)
         return -2;
       else
         {
           fprintf(stderr, "accept failed - %d. %s\n", errno, strerror(errno));
           return -1;
         }
     }
   set_nonblocking(s2);
   return s2;
 }

 static ___SCMOBJ c_recv(int s)
 {
   ___SCMOBJ ret;
   int n, done = 0;
   int len = 0;

   n = recv(s, &len, sizeof(len), 0);
   if (len == 0) return ___fix(-2);
   if (n != sizeof(int))
     {
       fprintf(stderr, "failed to recieve meta information - %d, %s\n", errno, strerror(errno));
       return ___FAL;
     }
   if (len <= C_RECV_BUFSZ)
     {
       char str[C_RECV_BUFSZ];
       n = recv(s, str, len, 0);
       if (n < 0)
         {
           if (errno == EAGAIN || errno == EWOULDBLOCK)
             return ___fix(-2);
           else
             return ___FAL;
         }
       else if (n == len)
         {
           done = 1;
           if (len < C_RECV_BUFSZ)
             memset(str + len, 0, C_RECV_BUFSZ - len);
         }
       ___nonnullcharstring_to_slogan_obj(str, &ret);
     }
   else
     {
       char *str = NULL;
       int received = 0;
       int rlen = C_RECV_BUFSZ;
       while (len > 0)
         {
           if (str == NULL)
             str = calloc(rlen, sizeof(char));
           else
             {
               str = realloc(str, received + rlen);
               memset(str + received, 0, rlen);
             }
           if (str == NULL)
             {
               fprintf(stderr, "failed to allocate receive buffer");
               return ___FAL;
             }
           n = recv(s, str + received, rlen, 0);
           if (n > 0)
             {
               int oldr = received;
               received += n;
               len -= n;
               if (len < C_RECV_BUFSZ) rlen = len;
             }
           if (n == 0 || len <= 0)
             {
               done = 1;
               ___nonnullcharstring_to_slogan_obj(str, &ret);
               free(str);
               break;
             }
           else if (n < 0)
             {
               if (errno != EAGAIN && errno != EWOULDBLOCK)
                 {
                   if (str != NULL) free(str);
                   return ___FAL;
                 }
             }
         }
     }
   if (done)
     return ___pair(___TRU, ret);
   else
     return ___pair(___FAL, ret);
 }

 static int c_send(int s, char *str, int len)
 {
   int n = 0;
   int orig_len = len;

   if (send(s, &len, sizeof(int), 0) != sizeof(int))
     {
       fprintf(stderr, "failed to send meta information - %d, %s\n", errno, strerror(errno));
       return -1;
     }
   if (len > C_RECV_BUFSZ)
     {
       int slen = C_RECV_BUFSZ;
       int tries = 0;

       while (len > 0)
         {
           n = send(s, str, slen, 0);
           if (n > 0 && n <= slen)
             {
               str += n;
               len -= n;
               if (len < slen) slen = len;
             }
           else if (errno != EAGAIN && errno != EWOULDBLOCK)
             break;
           if (tries >= 5)
             {
               sleep(1);
               tries = 0;
             }
           else ++tries;
         }
     }
   else n = send(s, str, len, 0);
   if (n < 0)
     {
       if (errno == EAGAIN || errno == EWOULDBLOCK)
         return -2;
       else
         return -1;
     }
   return orig_len;          
 }
 
c-declare-end
)

(define call-fork (c-lambda () int "fork"))
(define call-close (c-lambda (int) int "close"))
(define call-server-socket (c-lambda (char-string int) int "c_server_socket"))
(define call-client-socket (c-lambda (char-string) int "c_client_socket"))
(define call-accept (c-lambda (int) int "c_accept"))
(define call-recv (c-lambda (int) scheme-object "c_recv"))
(define call-send (c-lambda (int char-string int) int "c_send"))

(define *channel-count* 0)
(define *channel-name-mutex* (make-mutex))

(define (make-channel-name)
  (let ((name #f))
    (mutex-lock! *channel-name-mutex*)
    (set! name (string-append ".sn" (number->string *channel-count*) "ch"))
    (set! *channel-count* (+ 1 *channel-count*))
    (mutex-unlock! *channel-name-mutex*)
    name))

(define-structure process-info id socket child-socket)

(define (parent-process? pinfo)
  (> (process-info-id pinfo) 0))

(define process_id process-info-id)

(define (process_close pinfo)
  (if (process-info-child-socket pinfo)
      (begin (call-close (process-info-child-socket pinfo))
             (scm-kill (process-info-id pinfo) 9)))
  (zero? (call-close (process-info-socket pinfo))))

(define (invoke-child-callback cb sock)
  (with-exception-catcher
   (lambda (e)
     (call-close sock)
     (scm-println "exiting after exception - " e)
     (scm-exit 1))
   (lambda ()
     (cb (make-process-info 0 sock #f))
     (call-close sock)
     (scm-exit))))

(define *min-time-to-sleep* .05)

(define (process-accept sock #!optional timeout)
  (if (and timeout (scm-not (> timeout 0)))
      (error "process-accept - timeout must be a positive number."))
  (let loop ((child #f))
    (cond ((and timeout (<= timeout 0))
           #f)
          (else
           (set! child (call-accept sock))
           (cond ((= child -2)
                  (if timeout
                      (begin (thread-sleep! *min-time-to-sleep*)
                             (set! timeout (- timeout *min-time-to-sleep*)))
                      (thread-sleep! *min-time-to-sleep*))
                  (loop #f))
                 ((= child -1)
                  (error "process-accept failed"))
                 (else child))))))

(define (process child-callback #!optional timeout)
  (let* ((channel-name (make-channel-name))
         (parent-sock (call-server-socket channel-name 1)))
    (if (< parent-sock 0)
        (error "failed to create communication channel for parent process."))
    (let ((pid (call-fork)))
      (cond ((zero? pid)
             (call-close parent-sock)
             (let ((child-sock (call-client-socket channel-name)))
               (if (< child-sock 0)
                   (begin (scm-println "failed to create communication channel for client process, exiting.")
                          (scm-exit 1)))
               (invoke-child-callback child-callback child-sock)))
            ((> pid 0)
             (let ((child (process-accept parent-sock timeout)))
               (if (scm-not child)
                   (error "not able to contact child process.")
                   (make-process-info pid parent-sock child))))
            (else
             (call-close parent-sock)
             (error "process creation failed with error code - " pid))))))

(define (process_send pinfo object #!optional timeout)
  (if (and timeout (scm-not (> timeout 0)))
      (error "process-send - timeout must be a positive number."))
  (let ((channel (if (parent-process? pinfo)
                     (process-info-child-socket pinfo)
                     (process-info-socket pinfo)))
        (buf (open-output-string)))
    (scm-write object buf)
    (let* ((str (get-output-string buf))
           (len (string-length str)))
      (let loop ((r 0))
        (cond ((and timeout (<= timeout 0))
               (error 'timeout))
              (else
               (set! r (call-send channel str len))
               (cond ((> r 0)
                      (close-output-port buf)
                      r)
                     ((= r -2)
                      (if timeout
                          (begin (thread-sleep! *min-time-to-sleep*)
                                 (set! timeout (- timeout *min-time-to-sleep*)))
                          (thread-sleep! *min-time-to-sleep*))
                      (loop 0))
                     (else (close-output-port buf)
                           (error 'send_failed)))))))))

(define (process_receive pinfo #!optional timeout default)
  (if (and timeout (scm-not (> timeout 0)))
      (error "process-recv - timeout must be a positive number."))
  (let ((channel (if (parent-process? pinfo)
                     (process-info-child-socket pinfo)
                     (process-info-socket pinfo)))
        (buf (open-output-string)))
    (let loop ((object #f))
      (cond ((and timeout (<= timeout 0))
             default)
            (else
             (set! object (call-recv channel))
             (cond ((scm-not object)
                    (close-output-port buf)
                    (error 'receive_failed))
                   ((number? object)
                    (cond ((= -2 object)
                           (if timeout
                               (begin (thread-sleep! *min-time-to-sleep*)
                                      (set! timeout (- timeout *min-time-to-sleep*)))
                               (thread-sleep! *min-time-to-sleep*))
                           (loop #f))
                          (else (error "process-recv - unexpected return value - " object))))
                   ((pair? object)
                    (if (scm-not (scm-car object))
                        (begin (scm-print port: buf (scm-cdr object))
                               (loop #f))
                        (begin (scm-print port: buf (scm-cdr object))
                               (let ((inbuf (open-input-string (get-output-string buf))))
                                 (let ((r (scm-read inbuf)))
                                   (close-output-port buf)
                                   (close-input-port inbuf)
                                   r)))))
                   (else (error "process-recv - invalid return value - " object))))))))

(define (spawn child-callback #!key timeout
               send_timeout recv_timeout recv_default)
  (define (cb pinfo)
    (let ((message (process_receive pinfo recv_timeout recv_default)))
      (cond ((not (eq? message 'quit))
             (process_send pinfo (child-callback message) send_timeout)
             (cb pinfo)))))
  (let ((pinfo (process cb timeout)))
    (lambda (message)
      (cond ((eq? message 'info)
             pinfo)
            ((eq? message 'quit)
             (process_send pinfo message send_timeout)
             (process_close pinfo))
            (else
             (process_send pinfo message send_timeout)
             (let ((value *void*))
               (lambda (#!key timeout default)
                 (if (scm-eq? value *void*)
                     (set! value (process_receive pinfo timeout default)))
                 value)))))))

(define (process_terminate pinfo)
  (zero? (scm-kill (process-info-id pinfo) 9)))
