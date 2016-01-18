;; Copyright (c) 2013-2016 by Vijay Mathew Pandyalakal, All Rights Reserved.
;; Multi-core programming support.

;;;; The primitive `p-spawn` starts new child processes with a channel for communication
;;;; with the parent process. Parent and child processes are represented by the two callback
;;;; functions passed to `p-spawn`. The default channel allows very large messages to be exchanged
;;;; between the processes in arbitrary order. `p-spawn` can also work with `fast-channels` that
;;;; is suitable for problems that require the exchange of a lot of small messages.
;;;; The functions shown in the following examples are meant to be primitives on top of which
;;;; more powerful parallel programming constructs can be built. They are not meant for direct
;;;; consumption by application developers.

;;;; An example with the default channels:

;; define x = "hi!";
;; fn pcb(pinfo) { `p-broadcast`(pinfo, fn(pid) pid:"hello"); showln("parent got: " `p-receive`(pinfo)) };
;; fn ccb(pinfo) { let (d = `p-get`(`p-ichannel`(pinfo))) { showln("client got: " d); `p-put`(`p-ochannel`(pinfo), d) }};
;; // starts two child-processes
;; `p-spawn`(pcb ccb 2); 


;;;; Fast-channels are meant to be used in the request-reply pattern.
;;;; Parent and child callbacks has to agree on a protocol that makes
;;;; sure the communication start and flow in the correct sequence.

;; fn pcb(pinfo) { task_sleep(2); `p-broadcast`(pinfo, fn(pid) pid:"hello"); task_sleep(1); `p-receive`(pinfo) };
;; fn ccb(pinfo) { showln("child got - " let loop (r = false) { r = `p-get`(`p-ichannel`(pinfo), false, true); if (is_eof_object(r)) { task_sleep(.05); loop(r) } else r }); `p-put`(`p-ochannel`(pinfo), "thanks!", false, true) };
;; // starts 10 child-processes over a fast reply-request channel.
;; `p-spawn`(pcb ccb 10 true);

(c-declare #<<c-declare-end
 
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <semaphore.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include "../include/slogan.h"

#define SLOGAN_PIPE_BUFSZ 1024 * 8
 
 static ___SCMOBJ c_shm_open(char *smname, int smsz)
 {
   int r;
   int fd;

   if ((fd = shm_open(smname, O_CREAT | O_RDWR, S_IRWXU)) == -1)
     {
       fprintf(stderr, "c_shm_open - shm_open failed.\n");
       return ___FAL;
     }

   shm_unlink(smname);

   if ((r = ftruncate(fd, smsz)) != 0)
     {
       fprintf(stderr, "c_shm_open - ftruncate failed for size %d.\n", smsz);
       close(fd);
       shm_unlink(smname);
       return ___FAL;
     }
   return ___fix(fd);
 }

 static void *c_mmap(int fd, int smsz)
 {
   void *data;

   data = mmap(NULL, smsz, PROT_WRITE | PROT_READ, MAP_SHARED, fd, 0);
   if (data == MAP_FAILED)
     {
       fprintf(stderr, "mmap failed.\n");
       close(fd);
       return NULL;
     }
   return data;
 }

 static ___SCMOBJ c_munmap(void *data, int smsz)
 {
   if (munmap(data, smsz) != 0)
     {
       fprintf(stderr, "munmap failed.\n");
       return ___FAL;
     }
   return ___TRU;
 }

 static ___SCMOBJ c_msync(void *data, int smsz, int flag)
 {
   int f;

   switch(flag)
     {
     case 0: f = MS_ASYNC; break;
     case 1: f = MS_SYNC; break;
     case 2: f = MS_INVALIDATE; break;
     default:
       fprintf(stderr, "c_msync - invalid flag - %d.\n", flag);
       return ___FAL;
     }
   if (msync(data, smsz, f) != 0)
     {
       fprintf(stderr, "msync failed.\n");
       return ___FAL;
     }
   return ___TRU;
 }

 static ___SCMOBJ c_shm_write(int fd, void *data, ___SCMOBJ str, int strsz, int bufsz)
 {
   char *s;
   ___slogan_obj_to_nonnull_charstring(str, &s);
   if (strsz > bufsz)
     {
       if (ftruncate(fd, strsz) != 0)
         {
           fprintf(stderr, "c_shm_write - failed to allocate memory to write data %d.\n", strsz);
           return ___FAL;
         }
     }
   memcpy(data, s, strsz);
   return ___TRU;
 }

 static ___SCMOBJ c_shm_read(void *data)
 {
   ___SCMOBJ r;
   ___nonnullcharstring_to_slogan_obj((char *)data, &r);
   return r;
 }

 static void *c_sem_open(char *name, int creat, int c)
 {
   sem_t *s;
   if (creat)
     s = sem_open(name, O_CREAT | O_RDWR, 0644, c);
   else
     s = sem_open(name, O_RDWR);
   if (s == SEM_FAILED)
     {
       fprintf(stderr, "_sem_open failed.\n");
       return NULL;
     }
   return s;
 }

 static int c_sem_trywait(sem_t *s)
 {
   int r;

   r = sem_trywait(s);
   if (r == 0) return r;
   else if (r == -1)
     if (errno == EAGAIN)
       return -1;
   fprintf(stderr, "sem_trywait failed - %d.\n", errno);
   return -2;
 }

 static void set_nonblocking(int fd)
 {
   int flags = fcntl(fd, F_GETFL);
   if (fcntl(fd, F_SETFL, flags & ~O_NONBLOCK) == -1)
     fprintf(stderr, "set_nonblocking - fcntl failed for %d - %d.\n", fd, errno);
 }
 
 static ___SCMOBJ c_pipe()
 {
   int fds[2];
   
   if (pipe(fds) == -1)
     {
       fprintf(stderr, "pipe failed - %s.\n", strerror(errno));
       return ___FAL;
     }
   set_nonblocking(fds[0]);
   set_nonblocking(fds[1]);
   return ___pair(___fix(fds[0]), ___fix(fds[1]));
 }

 static ___SCMOBJ c_read(int fd)
 {
   char buf[SLOGAN_PIPE_BUFSZ];
   int r;
 read_again:
   r = read(fd, buf, SLOGAN_PIPE_BUFSZ);
   if (r == -1)
     {
       if (errno == EAGAIN || errno == EWOULDBLOCK)
         return ___fix(-1);
       else if (errno == EINTR)
         goto read_again;
       else
         fprintf(stderr, "read failed - %s.\n", strerror(errno));
     }
   else if (r == 0)
     return ___fix(0);
   else
     {
       ___SCMOBJ str;
       ___SCMOBJ ret;
       
       ___nonnullcharstring_to_slogan_obj(buf, &str);
       if (r < SLOGAN_PIPE_BUFSZ)
         ret = ___pair(___fix(0), str);
       else
         ret = ___pair(___fix(1), str);
       return ret;
     }
   return ___fix(-2);
 }

 static int c_write(int fd, ___SCMOBJ sbuf, int len)
 {
   char *buf;
   int r;

   ___slogan_obj_to_nonnull_charstring(sbuf, &buf);
 write_again:
   r = write(fd, buf, len);
   if (r == -1)
     {
       if (errno == EAGAIN || errno == EWOULDBLOCK)
         return -1;
       else if (errno == EINTR)
         goto write_again;
       else
         {
           fprintf(stderr, "write failed - %s.\n", strerror(errno));
           return -2;
         }
     }
   return r;
 }
   
c-declare-end
)

(c-define-type void-pointer (pointer void))
   
(define call-fork (c-lambda () int "fork"))
(define call-close (c-lambda (int) int "close"))
(define call-kill (c-lambda (int int) int "kill"))
(define call-pipe (c-lambda () scheme-object "c_pipe"))
(define call-write (c-lambda (int scheme-object int) int "c_write"))
(define call-read (c-lambda (int) scheme-object "c_read"))

(define call-shm-open (c-lambda (char-string int) scheme-object "c_shm_open"))
(define call-mmap (c-lambda (int int) void-pointer "c_mmap"))
(define call-munmap (c-lambda (void-pointer int) scheme-object "c_munmap"))
(define call-msync (c-lambda (void-pointer int int) scheme-object "c_msync"))
(define call-shm-unlink (c-lambda (char-string) int "shm_unlink"))
(define call-shm-write (c-lambda (int void-pointer scheme-object int int) scheme-object "c_shm_write"))
(define call-shm-read (c-lambda (void-pointer) scheme-object "c_shm_read"))
(define call-sem-open (c-lambda (char-string int int) void-pointer "c_sem_open"))
(define call-sem-wait (c-lambda (void-pointer) int "sem_wait"))
(define call-sem-trywait (c-lambda (void-pointer) int "c_sem_trywait"))
(define call-sem-post (c-lambda (void-pointer) int "sem_post"))
(define call-sem-close (c-lambda (void-pointer) int "sem_close"))

(define (shm-destroy fd bufname)
  (if fd
      (call-close fd))
  (call-shm-unlink bufname))

(define (mk-semname bn)
  (string-append bn "_lock"))

(define-structure fast-channel fd semaphore buffer name bufsz)
(define-structure io-channel in out)
(define-structure process-info fast? pids channels)

(define *bufnames* 0)
(define (new-bufname)
  (let ((s (string-append "/sloganprocs_" (number->string *bufnames*))))
    (set! *bufnames* (+ *bufnames* 1))
    s))

(define (proc-spawn-with-fast-channels parent-callback child-callback num-children bufsz)
  (let ((bufname (new-bufname)))
    (let loop ((i 0) (pids '()) (channels '()))
      (if (< i num-children)
          (let* ((bn (string-append bufname "_" (number->string i)))
                 (bn_p (string-append bn "_p"))
                 (bn_c (string-append bn "_c"))
                 (sn (mk-semname "_lock"))
                 (sn_p (string-append sn "_p"))
                 (sn_c (string-append sn "_c"))
                 (fd_p (call-shm-open bn_p bufsz))
                 (fd_c (call-shm-open bn_c bufsz)))
            (if (not fd_p)
                (error "failed to open parent communication channel"))
            (if (not fd_c)
                (error "failed to open child communication channel"))          
            (let ((data_p (call-mmap fd_p bufsz))
                  (data_c (call-mmap fd_c bufsz)))
              (if (or (not data_p) (not data_c))
                  (begin (shm-destroy fd_p bn_p)
                         (shm-destroy fd_c bn_c)
                         (error "failed to initialize queue for communication channels")))
              (let ((sem_p (call-sem-open sn_p 1 0))
                    (sem_c (call-sem-open sn_c 1 1)))
                (if (or (not sem_p) (not sem_c))
                    (begin (shm-destroy fd_p bn_p)
                           (shm-destroy fd_c bn_c)
                           (error "failed to create communcation channel sync objects - " sem_p ", " sem_c)))
                (let ((pid (call-fork)))
                  (cond ((< pid 0)
                         (shm-destroy fd_p bn_p)
                         (shm-destroy fd_c bn_c)
                         (error "failed to spawn child process"))
                        ((= pid 0)
                         (let ((sem_p (call-sem-open sn_p 0 1))
                               (sem_c (call-sem-open sn_c 0 0)))
                           (if (or (not sem_p) (not sem_c))
                               (error "failed to open sync objects - " sem_p ", " sem_c))
                           (child-callback
                            (make-process-info
                             #t #f
                             (make-io-channel
                              (make-fast-channel fd_p sem_p data_p bn_p bufsz)
                              (make-fast-channel fd_c sem_c data_c bn_c bufsz))))))
                        (else
                         (loop (+ i 1) (scm-cons pid pids)
                               (scm-cons
                                (make-io-channel
                                 (make-fast-channel fd_c sem_c data_c bn_c bufsz)
                                 (make-fast-channel fd_p sem_p data_p bn_p bufsz))
                                channels))))))))
          (parent-callback (make-process-info #t pids channels))))))

(define (kill-all pids)
  (let loop ((pids pids))
    (if (not (null? pids))
        (begin (call-kill (scm-car pids) 9)
               (loop (scm-cdr pids))))))

(define (close-all-pipes fds)
  (let loop ((fds fds))
    (if (not (null? fds))
        (begin (call-close (scm-caar fds))
               (call-close (scm-cdar fds))
               (loop (scm-cdr fds))))))

(define (proc-spawn parent-callback child-callback num-children
                    fast-channels #!optional (fast-channel-bufsz 1024))
  (if fast-channels
      (proc-spawn-with-fast-channels parent-callback child-callback num-children fast-channel-bufsz)
      (let loop ((i 0) (pids '()) (pipe-fds '()))
        (if (< i num-children)
            (let ((fds_p (call-pipe))
                  (fds_c (call-pipe)))
              (if (not fds_p)
                  (error "proc-spawn - failed to create parent communication channel."))
              (if (not fds_c)
                  (error "proc-spawn - failed to create child communication channel."))
              (let ((pid (call-fork)))
                (cond ((zero? pid)
                       (call-close (scm-cdr fds_p))
                       (call-close (scm-car fds_c))
                       (child-callback (make-process-info #f #f (make-io-channel (scm-car fds_p) (scm-cdr fds_c)))))
                      ((> pid 0)
                       (call-close (scm-car fds_p))
                       (call-close (scm-cdr fds_c))
                       (loop (+ i 1) (scm-cons pid pids) (scm-cons (make-io-channel (scm-car fds_c) (scm-cdr fds_p)) pipe-fds)))
                      (else
                       (kill-all pids)
                       (close-all-pipes (scm-cons fds_p (scm-cons fds_c pipe-fds)))
                       (error "proc-spawn - failed to start child process.")))))
            (parent-callback (make-process-info #f pids pipe-fds))))))

(define *proc-io-min-timeout* .05) ;; 50 milliseconds

(define (proc-read fd timeout)
  (let loop ((r (call-read fd)) (s ""))
    (if (and timeout (< timeout *proc-io-min-timeout*))
        (error 'timeout))
    (cond ((pair? r)
           (if (zero? (scm-car r))
               (string-append s (scm-cdr r))
               (loop (call-read fd) (string-append s (scm-cdr r)))))
          ((zero? r) s)
          ((= r -1) 
           (if (and timeout (> timeout *proc-io-min-timeout*))
               (begin (thread-sleep! *proc-io-min-timeout*)
                      (set! timeout (- timeout *proc-io-min-timeout*))))
           (loop (call-read fd) s))
          (else (error "proc-read failed - " r)))))

(define (proc-write fd str timeout)
  (let loop ((str str)
             (len (string-length str)))
    (if (and timeout (< timeout *proc-io-min-timeout*))
        (error 'timeout))
    (let ((r (call-write fd str len)))
      (cond ((or (zero? r) (= r len))
             #t)
            ((= r -1)
             (if (and timeout (> timeout *proc-io-min-timeout*))
                 (begin (thread-sleep! *proc-io-min-timeout*)
                        (set! timeout (- timeout *proc-io-min-timeout*))))
             (loop (substring str r len) (- len r)))
            (else (error "proc-write failed - " r))))))

(define (proc-write-fast channel str #!optional timeout)
  (let* ((bufsz (fast-channel-bufsz channel))
         (len (string-length str))
         (new-bufsz (if (> len bufsz) len bufsz))
         (sem (fast-channel-semaphore channel)))
    (let loop ((e 0))
      (if (and timeout (< timeout *proc-io-min-timeout*))
          (error 'timeout))
      (set! e ((if timeout call-sem-trywait call-sem-wait) sem))
      (cond ((zero? e)
             (let ((r (call-shm-write (fast-channel-fd channel) 
                                      (fast-channel-buffer channel)
                                      str len new-bufsz)))
               (if (not r)
                   (error "proc-write-fast failed."))
               (call-sem-post sem)
               (if (> new-bufsz bufsz)
                   (fast-channel-bufsz-set! channel new-bufsz))
               r))
            ((= e -1)
             (if (and timeout (> timeout *proc-io-min-timeout*))
                 (begin (set! timeout (- timeout *proc-io-min-timeout*))
                        (thread-sleep! *proc-io-min-timeout*))
                 (error (cons "write" 'semaphore)))
             (loop e))
            (else (error "proc-write-fast failed to acquire lock- " e))))))

(define (proc-read-fast channel #!optional timeout)
  (let ((sem (fast-channel-semaphore channel))
        (buf (fast-channel-buffer channel)))
    (let loop ((e 0))
      (if (and timeout (< timeout *proc-io-min-timeout*))
          (error 'timeout))
      (set! e ((if timeout call-sem-trywait call-sem-wait) sem))
      (cond ((zero? e)
             (let ((str (call-shm-read buf)))
               (call-sem-post sem)
               (cond ((= 0 (string-length str))
                      (if (and timeout (> timeout *proc-io-min-timeout*))
                          (begin (thread-sleep! *proc-io-min-timeout*)
                                 (set! timeout (- timeout *proc-io-min-timeout*))))
                      (loop e))
                     (else str))))
            ((= e -1)
             (if (and timeout (> timeout *proc-io-min-timeout*))
                 (begin (set! timeout (- timeout *proc-io-min-timeout*))
                        (thread-sleep! *proc-io-min-timeout*))
                 (error (cons "read" 'semaphore)))
             (loop e))
            (else (error "proc-read-fast failed to acquire lock- " e))))))

(define (parent-done pinfo)
  (map (lambda (fdo fdi)
         (call-close fdo)
         (call-close fdi))
       (p-ochannels pinfo)
       (p-ichannels pinfo)))

(define (parent-fast-done pinfo)
  (map (lambda (ich och)
         (call-msync (fast-channel-buffer ich)
                     (fast-channel-bufsz ich) 1)
         (call-msync (fast-channel-buffer och)
                     (fast-channel-bufsz och) 1)         
         (call-munmap (fast-channel-buffer ich)
                      (fast-channel-bufsz ich))
         (call-munmap (fast-channel-buffer och)
                      (fast-channel-bufsz och))
         (shm-destroy (fast-channel-fd ich)
                      (fast-channel-name ich))
         (shm-destroy (fast-channel-fd och)
                      (fast-channel-name och)))
       (p-ichannels pinfo)
       (p-ochannels pinfo)))

(define (child-done pinfo)
  (call-close (p-ochannel pinfo))
  (call-close (p-ichannel pinfo))
  (exit))

(define (child-fast-done pinfo)
  (let ((och (p-ochannel pinfo))
        (ich (p-ichannel pinfo)))
    (shm-destroy (fast-channel-fd och)
                 (fast-channel-name och))
    (shm-destroy (fast-channel-fd ich)
                 (fast-channel-name ich)))
  (exit))

(define parent-process? process-info-pids)

(define (process-info-close pinfo)
  (let ((parent? (parent-process? pinfo))
        (fast? (process-info-fast? pinfo)))
    (if parent?
        ((if fast? parent-fast-done parent-done)
         pinfo)
        ((if fast? child-fast-done child-done)
         pinfo))))

(define (p-spawn parent-callback child-callback
                 #!optional (num-children 1)
                 fast-channels (fast-channel-bufsz 1024))
  (if (< num-children 1)
      (error "p-spawn - number of child processes must be a positive number."))
  (let ((p-done (if fast-channels parent-fast-done parent-done))
        (c-done (if fast-channels child-fast-done child-done)))
    (let ((pcb (if parent-callback
                   (lambda (pinfo)
                     (with-exception-catcher
                      (lambda (e)
                        (p-done pinfo) (raise e))
                      (lambda ()
                        (let ((r (parent-callback pinfo)))
                          (p-done pinfo)
                          r))))
                   scm-identity))
          (ccb (if child-callback
                   (lambda (pinfo)
                     (with-exception-catcher
                      (lambda (e)
                        (c-done pinfo))
                      (lambda ()
                        (child-callback pinfo)
                        (c-done pinfo))))
                   scm-identity)))
      (proc-spawn pcb ccb num-children fast-channels))))

(define (p-put channel obj #!optional timeout fast-channel)
  (if (and timeout (not (> timeout 0)))
      (error "p-put - timeout must be a positive number."))
  (let ((buf (open-output-string)))
    (scm-write obj buf)
    (with-exception-catcher
     (lambda (e)
       (close-output-port buf)
       (raise e))
     (lambda ()
       ((if fast-channel
            proc-write-fast
            proc-write)
        channel (get-output-string buf) timeout)))
    #t))

(define (p-get channel #!optional timeout fast-channel)
  (if (and timeout (not (> timeout 0)))
      (error "p-get - timeout must be a positive number."))
  (let ((str ((if fast-channel proc-read-fast proc-read) channel timeout)))
    (let ((buf (open-input-string str)))
      (let ((obj (scm-read buf)))
        (close-input-port buf)
        obj))))

(define (p-broadcast pinfo putfn #!optional timeout)
  (map (lambda (pid ch)
         (p-put ch (putfn pid) timeout (process-info-fast? pinfo)))
       (process-info-pids pinfo) (p-ochannels pinfo)))

(define (p-receive pinfo #!optional timeout getfn default-value)
  (map (lambda (pid ch) (if getfn
                            (if (getfn pid)
                                (p-get ch timeout (process-info-fast? pinfo))
                                default-value)
                            (p-get ch timeout (process-info-fast? pinfo))))
       (process-info-pids pinfo) (p-ichannels pinfo)))

;; To be called by a child process
(define (p-ichannel pinfo) (io-channel-in (process-info-channels pinfo)))
(define (p-ochannel pinfo) (io-channel-out (process-info-channels pinfo)))

;; To be called by a parent process
(define (p-ochannels pinfo) (map io-channel-out (process-info-channels pinfo)))
(define (p-ichannels pinfo) (map io-channel-in (process-info-channels pinfo)))

(define default-channel-options (list (cons 'buffer_size 1024)))

(define (process child_callback #!key parent_callback 
                 (count 1) (use_fast_channel #t)
                 (channel_options default-channel-options))
  (p-spawn parent_callback child_callback count
           use_fast_channel
           (if channel_options
               (assoc 'buffer_size channel_options)
               #f)))

(define is_parent_process parent-process?)

(define (process_send pinfo obj #!key timeout default_value)
  (let ((parent? (parent-process? pinfo))
        (fast? (process-info-fast? pinfo)))
    (with-exception-catcher
     (lambda (e)
       (if (eq? e 'timeout)
           default_value
           (raise e)))
     (lambda ()
       (if parent?
           (map (lambda (ch) (p-put ch obj timeout fast?))
                (p-ochannels pinfo))
           (p-put (p-ochannel pinfo) obj timeout fast?))))))

(define (process_receive pinfo #!key timeout default_value)
  (let ((parent? (parent-process? pinfo))
        (fast? (process-info-fast? pinfo)))
    (with-exception-catcher
     (lambda (e)
       (if (eq? e 'timeout)
           default_value
           (raise e)))
     (lambda ()
       (if parent?
           (map (lambda (ch) (p-get ch timeout fast?))
                (p-ichannels pinfo))
           (p-get (p-ichannel pinfo) timeout fast?))))))

(define proc-kill kill)

(define (process_kill pinfo)
  (if (parent-process? pinfo)
      (map (lambda (pid) (proc-kill pid 9)) (process-info-pids pinfo))
      #f))

