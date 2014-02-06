;; Copyright (c) 2013-2014 by Vijay Mathew Pandyalakal, All Rights Reserved.

(define (stream type path-or-settings)
  ((case type
     ((file) open-file)
     ((process) open-process)
     ((tcp_client) open-tcp-client)
     ((tcp_server) open-tcp-server)
     ((directory) open-directory)
     ((array) open-vector)
     ((byte_array) open-u8vector)
     ((string) open-string)
     (else invalid-stream-type))
   (slgn-path/settings->scm-path/settings path-or-settings)))

(define (with_stream type path-or-settings fn)
  (let ((s (stream type path-or-settings)))
    (with-exception-catcher 
     (lambda (e)
       (if s (close-port s))
       (raise e))
     (lambda ()
       (let ((val (fn s)))
         (close-port s)
         (set! s #f)
         val)))))
    
(define (with_input_stream type path-or-settings fn)
  ((case type
     ((file) call-with-input-file)
     ((process) call-with-input-process)
     ((array) call-with-input-vector)
     ((byte_array) call-with-input-u8vector)
     ((string) call-with-input-string)
     (else invalid-stream-type))
   (slgn-path/settings->scm-path/settings path-or-settings)
   fn))

(define (with_output_stream type path-or-settings fn)
  ((case type
     ((file) call-with-output-file)
     ((process) call-with-output-process)
     ((array) call-with-output-vector)
     ((byte_array) call-with-output-u8vector)
     ((string) call-with-output-string)
     (else invalid-stream-type))
   (slgn-path/settings->scm-path/settings path-or-settings)
   fn))

(define (invalid-stream-type #!rest args)
  (error "not a supported stream type. " args))

(define process_pid process-pid)
(define process_status process-status)

(define (tcp_service_register path-or-settings fn) 
  (tcp-service-register! (slgn-path/settings->scm-path/settings path-or-settings) fn))

(define (tcp_service_unregister path-or-settings)
  (tcp-service-unregister! (slgn-path/settings->scm-path/settings path-or-settings)))

(define is_input_stream input-port?)
(define is_output_stream output-port?)
(define is_stream port?)
(define current_input_stream current-input-port)
(define current_output_stream current-output-port)
(define close_input_stream close-input-port)
(define close_output_stream close-output-port)
(define close_stream close-port)
(define read_char read-char)
(define peek_char peek-char)
(define is_char_ready char-ready?)
(define write_char write-char)
(define read_line read-line)
(define read_substring read-substring)
(define write_substring write-substring)
(define read_byte read-u8)
(define write_byte write-u8)
(define get_output_string get-output-string)
(define get_output_byte_array get-output-u8vector)
(define get_output_array get-output-vector)

(define (read_bytes bytes #!optional (start 0) end 
                    (port (current-input-port)) 
                    need)
  (if (not end) (set! end (u8vector-length bytes))) 
  (read-subu8vector bytes start end port need))

(define (write_bytes bytes #!optional (start 0) end 
                     (port (current-output-port)))
  (if (not end) (set! end (u8vector-length bytes)))
  (write-subu8vector bytes start end port))

(define force_output force-output)
(define is_eos eof-object?)
(define input_stream_byte_position input-port-byte-position)
(define output_stream_byte_position output-port-byte-position)
(define scm_print print)
(define scm_println println)

(define (print #!key (to (current-output-port)) #!rest args)
  (let loop ((args args))
    (if (not (null? args))
        (begin (slgn-display (car args) display-string: #t port: to)
               (loop (cdr args))))))

(define (println #!key (to (current-output-port)) #!rest args)
  (apply print (append (list to: to) args))
  (newline to))

(define (read_expression #!optional (from (current-input-port)))
  (let ((tokenizer (make-tokenizer from)))
    (expression tokenizer)))

(define (read_program #!optional (from (current-input-port)))
  (let ((tokenizer (make-tokenizer from)))
    (slogan tokenizer)))

(define (write_expression obj #!optional (to (current-output-port)))
  (slgn-display obj port: to))
