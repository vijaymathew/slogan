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

(define (slgn-path/settings->scm-path/settings path-or-settings)
  (if (string? path-or-settings)
      path-or-settings
      (let loop ((settings path-or-settings)
                 (result '()))
        (if (null? settings)
            result
            (loop (cdr settings)
                  (append (list (slgn-symbol->scm-keyword (car (car settings)))
                                (slgn-setting->scm-setting (cdr (car settings))))
                          result))))))

(define (slgn-setting->scm-setting s)
  (if (symbol? s)
      (slgn-symbol->scm-symbol s)
      s))

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
(define read_byte_array read-subu8vector)
(define write_byte_array write-subu8vector)
(define force_output force-output)
(define is_eos eof-object?)

(define scm_print print)
(define scm_println println)

(define (print #!key (output (current-output-port)) #!rest args)
  (let loop ((args args))
    (if (not (null? args))
        (begin (slgn-display (car args) display-string: #t port: output)
               (loop (cdr args))))))

(define (println #!key (output (current-output-port)) #!rest args)
  (apply print (append (list output: output) args))
  (newline output))

(define (read_expression #!key (input (current-input-port)))
  (let ((tokenizer (make-tokenizer input)))
    (expression tokenizer)))

(define (read_program #!key (input (current-input-port)))
  (let ((tokenizer (make-tokenizer input)))
    (slogan tokenizer)))

(define (write_expression obj #!key (output (current-output-port)))
  (slgn-display obj port: output))
