;; Copyright (c) 2013-2014 by Vijay Mathew Pandyalakal, All Rights Reserved.

(c-declare #<<c-declare-end
#include <string.h>

#include "nn.h"
#include "pubsub.h"
#include "pipeline.h"
#include "reqrep.h"
#include "bus.h"
#include "survey.h"
#include "pair.h"

 char *nn_str_error (int n)
 {
   return (char *) nn_strerror (n);
 }

 int is_eagain () 
 {
   return (nn_errno () == EAGAIN);
 }

 int c_AF_SP () { return AF_SP; }
 int c_AF_SP_RAW () { return AF_SP_RAW; }
 int c_NN_PAIR () { return NN_PAIR; }
 int c_NN_PUB () { return NN_PUB; }
 int c_NN_SUB () { return NN_SUB; }
 int c_NN_SUB_SUBSCRIBE () { return NN_SUB_SUBSCRIBE; }
 int c_NN_SUB_UNSUBSCRIBE () { return NN_SUB_UNSUBSCRIBE; }
 int c_NN_BUS () { return NN_BUS; }
 int c_NN_REQ () { return NN_REQ; }
 int c_NN_REP () { return NN_REP; }
 int c_NN_REQ_RESEND_IVL () { return NN_REQ_RESEND_IVL; }
 int c_NN_PUSH () { return NN_PUSH; }
 int c_NN_PULL () { return NN_PULL; }
 int c_NN_SURVEYOR () { return NN_SURVEYOR; }
 int c_NN_RESPONDENT () { return NN_RESPONDENT; }
 int c_NN_SURVEYOR_DEADLINE () { return NN_SURVEYOR_DEADLINE; }

 int c_NN_DONTWAIT () { return NN_DONTWAIT; }

 int nn_send_bytes (int fd, ___SCMOBJ u8vec_data, int len, int flags)
 {
   return send (fd, ___BODY (u8vec_data), len, flags);
 }

 int nn_send_string (int fd, ___SCMOBJ str, int len, int flags)
 {
   char *s;
   int r;
   
   ___SCMOBJ_to_NONNULLCHARSTRING (str, &s, ___RETURN_POS);
   r = nn_send (fd, s, len, flags);
   ___release_string (s);
   return r;
 }

 int nn_recv_bytes (int fd, ___SCMOBJ u8vec_data, int len, int flags)
 {
   return nn_recv (fd, ___BODY (u8vec_data), len, flags);
 }

 ___SCMOBJ nn_recv_string (int fd, int len, int flags)
 {
   char *s = nn_allocmsg (len, 0);
   int r;
   ___SCMOBJ result;
   
   if (!s) return ___FAL;
   memset (s, 0, len);
   r = nn_recv (fd, s, len, flags);
   if (r == 0) result = ___TRU;
   else if (r < 0) result = ___FAL;
   else
     {
       ___NONNULLCHARSTRING_to_SCMOBJ (___PSTATE, s, &result, ___RETURN_POS);
       ___release_scmobj (result);
     }
   nn_freemsg (s);
   return result;
 }

c-declare-end
)

 (define nn_strerror (c-lambda (int) char-string "nn_str_error"))
 (define nn_errno (c-lambda () int "nn_errno"))
 (define eagain? (c-lambda () int "is_eagain"))

 (define c-AF_SP (c-lambda () int "c_AF_SP"))
 (define c-AF_SP_RAW (c-lambda () int "c_AF_SP_RAW"))
 (define c-NN_PAIR (c-lambda () int "c_NN_PAIR"))
 (define c-NN_PUB (c-lambda () int "c_NN_PUB"))
 (define c-NN_SUB (c-lambda () int "c_NN_SUB"))
 (define c-NN_SUB_SUBSCRIBE (c-lambda () int "c_NN_SUB_SUBSCRIBE"))
 (define c-NN_SUB_UNSUBSCRIBE (c-lambda () int "c_NN_SUB_UNSUBSCRIBE"))
 (define c-NN_BUS (c-lambda () int "c_NN_BUS"))
 (define c-NN_REQ (c-lambda () int "c_NN_REQ"))
 (define c-NN_REP (c-lambda () int "c_NN_REP"))
 (define c-NN_REQ_RESEND_IVL (c-lambda () int "c_NN_REQ_RESEND_IVL"))
 (define c-NN_PUSH (c-lambda () int "c_NN_PUSH"))
 (define c-NN_PULL (c-lambda () int "c_NN_PULL"))
 (define c-NN_SURVEYOR (c-lambda () int "c_NN_SURVEYOR"))
 (define c-NN_RESPONDENT (c-lambda () int "c_NN_RESPONDENT"))
 (define c-NN_SURVEYOR_DEADLINE (c-lambda () int "c_NN_SURVEYOR_DEADLINE"))

 (define c-NN_DONTWAIT (c-lambda () int "c_NN_DONTWAIT"))
 
 (define c-AF-SP (c-AF_SP))
 (define c-AF-SP-RAW (c-AF_SP_RAW))
 (define c-NN-PAIR (c-NN_PAIR))
 (define c-NN-PUB (c-NN_PUB))
 (define c-NN-SUB (c-NN_SUB))
 (define c-NN-PUB-SUBSCRIBE (c-NN_SUB_SUBSCRIBE))
 (define c-NN-PUB-UNSUBSCRIBE (c-NN_SUB_UNSUBSCRIBE))
 (define c-NN-BUS (c-NN_BUS))
 (define c-NN-REQ (c-NN_REQ))
 (define c-NN-REP (c-NN_REP))
 (define c-NN-REQ-RESEND-IVL (c-NN_REQ_RESEND_IVL))
 (define c-NN-PUSH (c-NN_PUSH))
 (define c-NN-PULL (c-NN_PULL))
 (define c-NN-SURVEYOR (c-NN_SURVEYOR))
 (define c-NN-RESPONDENT (c-NN_RESPONDENT))
 (define c-NN-SURVEYOR-DEADLINE (c-NN_SURVEYOR_DEADLINE))

 (define c-NN-DONTWAIT (c-NN_DONTWAIT))

 (define nn_socket (c-lambda (int int) int "nn_socket"))
 (define nn_close (c-lambda (int) int "nn_close"))
 (define nn_bind (c-lambda (int char-string) int "nn_bind"))
 (define nn_connect (c-lambda (int char-string) int "nn_connect"))
 (define nn_shutdown (c-lambda (int int) int "nn_shutdown"))
 (define nn_send_bytes (c-lambda (int scheme-object int int) int "nn_send_bytes"))
 (define nn_send_string (c-lambda (int scheme-object int int) int "nn_send_string"))
 (define nn_recv_bytes (c-lambda (int scheme-object int int) int "nn_recv_bytes"))
 (define nn_recv_string (c-lambda (int int int) scheme-object "nn_recv_string"))

 (define nn_device (c-lambda (int int) int "nn_device"))
 (define nn_term (c-lambda () void "nn_term"))

 (define (socket-type->nn-const type)
   (if (integer? type)
       type
       (case type
         ((!pair) c-NN-PAIR)
         ((!pub) c-NN-PUB)
         ((!sub) c-NN-SUB)
         ((!bus) c-NN-BUS)
         ((!req) c-NN-REQ)
         ((!rep) c-NN-REP)
         ((!push) c-NN-PUSH)
         ((!pull) c-NN-PULL)
         ((!surveyor) c-NN-SURVEYOR)
         ((!respondent) c-NN-RESPONDENT)
         (else (error "invalid socket type. " type)))))
         
 (define (handle-syserror r)
   (if (< r 0)
       (error (nn_strerror (nn_errno)))
       r))

 (define (msg_socket type)
   (handle-syserror (nn_socket c-AF-SP (socket-type->nn-const type))))

 (define (msg_raw_socket type)
   (handle-syserror (nn_socket c-AF-SP-RAW (socket-type->nn-const type))))
 
 (define (msg_close s)
   (>= 0 (nn_close s)))

 (define (msg_bind s addr)
   (handle-syserror (nn_bind s addr)))

 (define (msg_connect s addr)
   (handle-syserror (nn_connect s addr)))

 (define (msg_shutdown s how)
   (>= 0 (nn_shutdown s how)))

 (define (msg_send s obj #!key (dont_wait #t))
   (let ((isstr (string? obj)))
     (let ((len (if isstr
                    (string-length obj)
                    (u8vector-length obj)))
           (flags (if dont_wait c-NN-DONTWAIT 0)))
       (let ((r ((if isstr nn_send_string nn_send_bytes) s obj len flags)))
         (if (<= r 0)
             (eagain?)
             r)))))                 
 
 (define (msg-recv-bytes s len dont-wait)
   (let ((vect (make-u8vector len)))
     (if (<= (nn_recv_bytes s vect len (if dont-wait c-NN-DONTWAIT 0))
             0)
         #f
         vect)))

 (define (msg_recv s len #!key 
                   (dont_wait #t)
                   (binary #f))
   (let ((r (if binary
                (msg-recv-bytes s len dont_wait)
                (nn_recv_string len (if dont_wait c-NN-DONTWAIT 0)))))
     (if (not r)
         (eagain?)
         r)))

 (define (msg_device a b)
   (handle-syserror (nn_device a b)))

 (define msg_term nn_term)
