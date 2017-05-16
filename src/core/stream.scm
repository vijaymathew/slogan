;; Copyright (c) 2013-2017 by Vijay Mathew Pandyalakal, All Rights Reserved.

(define-structure text-transcoder codec eol-style error-handling-mode)

(define *supported-codecs* '(ISO_8859_1 
                             ASCII UTF_8 UTF_16 UTF_16LE
                             UTF_16BE UCS_2 UCS_2LE UCS_2BE 
			     UCS_4 UCS_4LE UCS_4BE))

(define (validate-codec codec)
  (if (scm-not (symbol? codec))
      (scm-error "codec must be a symbol"))
  (if (scm-not (scm-memq (string->symbol (string_upcase (symbol->string codec)))
                 *supported-codecs*))
      (scm-error "unsupported codec" codec))
  codec)

(define *error-modes* '(replace raise ignore))

(define (validate-error-handling-mode mode)
  (if (scm-memq mode *error-modes*) mode
      (scm-error "invalid error handling mode" mode)))

(define *eol-styles* '(lf cr crlf))

(define (validate-eol-style eol-style)
  (if (scm-memq eol-style *eol-styles*) eol-style
      (scm-error "unsupported eol-style" eol-style)))

(define (error-handling-mode->boolean mode) (scm-eq? mode 'replace))

(define (eol-style->encoding eol-style)
  (if (scm-eq? eol-style 'crlf) 'cr-lf
      eol-style))

(define (transcoder codec #!optional (eol-style 'crlf) (error-handling-mode 'ignore))
  (make-text-transcoder (validate-codec codec) (validate-eol-style eol-style) 
                        (validate-error-handling-mode error-handling-mode)))

(define (transcoder_codec t) (text-transcoder-codec t))
(define (transcoder_eol_style t) (text-transcoder-eol-style t))
(define (transcoder_error_handling_mode t) (text-transcoder-error-handling-mode t))

(define (get-stream-option options key #!optional default)
  (if (list? options)
      (if (scm-memq key options) #t
          default)
      default))

(define (get-codec-from-transcoder tcoder)
  (if tcoder (text-transcoder-codec tcoder)
      #f))

(define (get-error-handling-mode-from-transcoder tcoder)
  (if tcoder 
      (let ((em (text-transcoder-error-handling-mode tcoder)))
	(if (scm-eq? em 'ignore) #f
	    (error-handling-mode->boolean em)))
      #f))

(define (get-eol-encoding-from-transcoder tcoder)
  (if tcoder (eol-style->encoding (text-transcoder-eol-style tcoder))
      'cr-lf))

(define (get-buffering-for-stream bmode)
  (if bmode
      (case bmode
        ((block) #t)
        ((none) #f)
        ((line) 'line)
        (else (scm-error "invalid buffer-mode" bmode)))
      bmode))

(define (replace-underscores sym) (slgn-symbol->scm-sym/kw sym string->symbol))

(define (open-file-stream-helper path direction options buffer-mode tcoder permissions)
  (let ((settings (scm-list path: path direction: direction)))
    (let ((opt (get-stream-option options 'append)))
      (if opt (set! settings (scm-append settings (scm-list append: opt)))))
    (if (scm-not (scm-eq? direction 'input))
        (let ((opt (get-stream-option options 'create)))
          (if opt (set! settings (scm-append settings (scm-list create: #t)))
              (let ((opt (get-stream-option options 'no_create)))
                (if opt (set! settings (scm-append settings (scm-list create: #f)))
                    (let ((opt (get-stream-option options 'maybe_create)))
                      (if opt (set! settings (scm-append settings (scm-list create: 'maybe))))))))))
    (let ((opt (get-stream-option options 'truncate)))
      (if opt (set! settings (scm-append settings (scm-list truncate: opt)))))
    (let ((opt (get-codec-from-transcoder tcoder)))
      (if opt (set! settings (scm-append settings (scm-list char-encoding: (replace-underscores opt))))))
    (let ((opt (get-error-handling-mode-from-transcoder tcoder)))
      (if opt (set! settings (scm-append settings (scm-list char-encoding-errors: opt)))))
    (let ((opt (get-eol-encoding-from-transcoder tcoder)))
      (if opt (set! settings (scm-append settings (scm-list eol-encoding: opt)))))
    (if permissions (set! settings (scm-append settings (scm-list permissions: permissions))))
    (open-file (scm-append settings (scm-list buffering: (get-buffering-for-stream buffer-mode))))))

(define (file_reader path #!key options buffer_mode transcoder)
  (open-file-stream-helper path 'input options buffer_mode transcoder #f))

(define (file_writer path #!key options buffer_mode transcoder (permissions #o666))
  (open-file-stream-helper path 'output options buffer_mode transcoder permissions))

(define (file_stream path #!key options buffer_mode transcoder (permissions #o666))
  (open-file-stream-helper path 'input-output options buffer_mode transcoder permissions))

(define current_reader current-input-port)
(define current_writer current-output-port)
(define current_error_stream current-error-port)

(define (open-byte-stream-helper openfn invalue tcoder)
  (if (scm-not tcoder)
      (openfn invalue)
      (let ((settings (scm-list init: invalue)))
        (let ((opt (get-codec-from-transcoder tcoder)))
          (if opt (set! settings (scm-append settings (scm-list char-encoding: (replace-underscores opt))))))
        (let ((opt (get-error-handling-mode-from-transcoder tcoder)))
          (if opt (set! settings (scm-append settings (scm-list char-encoding-errors: opt)))))
        (let ((opt (get-eol-encoding-from-transcoder tcoder)))
          (if opt (set! settings (scm-append settings (scm-list eol-encoding: opt)))))
        (openfn settings))))

(define (slgn-byte_array_reader byte_array #!key transcoder)
  (open-byte-stream-helper open-input-u8vector byte_array transcoder))

(define byte_array_reader slgn-byte_array_reader)

(define (byte_array_writer #!key transcoder)
  (open-byte-stream-helper open-output-u8vector '() transcoder))

(define (string_reader str)
  (open-byte-stream-helper open-input-string str #f))

(define (string_writer #!optional (s *void*))
  (let ((stream (open-byte-stream-helper open-output-string '() #f)))
    (if (scm-not (void? s)) (scm-show stream: stream s))
    stream))

(define get_output_string get-output-string)
(define get_output_bytes get-output-u8vector)

(define (is_stream obj)
  (or (port? obj) (bits-stream? obj)))

(define (is_reader obj)
  (or (input-port? obj) (bits-reader-info? obj)))

(define (is_writer obj)
  (or (output-port? obj) (bits-writer-info? obj)))

(define (close_stream obj)
  (if (port? obj)
      (close-port obj)
      (if (scm-not (bits-stream? obj))
          (scm-error "invalid stream object" obj))))

(define (close_reader obj)
  (if (input-port? obj)
      (close-input-port obj)
      (if (scm-not (bits-reader-info? obj))
          (scm-not "invalid input stream object" obj))))

(define (close_writer obj)
  (if (output-port? obj)
      (close-output-port obj)
      (if (scm-not (bits-writer-info? obj))
          (scm-not "invalid output stream object" obj))))

(define (stream_position p)
  (if (input-port? p)
      (input-port-byte-position p)
      (output-port-byte-position p)))

(define (stream_has_position p)
  (with-exception-catcher 
   (lambda (e) #f)
   (lambda () (if (stream_position p) #t #f))))

(define (whence->integer w)
  (case w
    ((beginning) 0)
    ((current) 1)
    ((end) 2)
    (else (scm-error "invalid whence for stream position change" w))))

(define (set_stream_position p pos #!optional (whence 'beginning))
  (if (input-port? p)
      (input-port-byte-position p pos (whence->integer whence))
      (output-port-byte-position p pos (whence->integer whence))))

(define (safely-close-stream p)
  (if (port? p)
      (with-exception-catcher
       (lambda (e) #f)
       (lambda () (close-port p)))))

(define (call_with_stream p proc)
  (with-exception-catcher
   (lambda (e)
     (safely-close-stream p)
     (scm-raise e))
   (lambda () 
     (let ((r (proc p))) 
       (safely-close-stream p)
       r))))

(define (eof_object) #!eof)
(define is_eof_object eof-object?)

(define *def-buf-sz* 1024)

(define read_byte read-u8)

(define (read-n-helper p n initfn rdfn subfn)
  (let ((r (initfn n)))
    (let ((n (rdfn r 0 n p n)))
      (if (zero? n) #!eof
          (subfn r 0 n)))))
  
(define (check-array-for-eof arr lenfn)
  (if (or (eof-object? arr) (zero? (lenfn arr))) #!eof arr))

(define (read_n_bytes n #!optional (p (current-input-port)))
  (check-array-for-eof 
   (read-n-helper p n make-u8vector read-subu8vector scm-subu8vector)
   u8vector-length))

(define (read-all-helper p init rdfn apndfn bufsz)
  (let loop ((r init)
             (nr (rdfn bufsz p)))
    (if (eof-object? nr) r
        (loop (apndfn r nr)
              (rdfn bufsz p)))))

(define (read_all_bytes #!optional (p (current-input-port)) (bufsz *def-buf-sz*))
  (check-array-for-eof 
   (read-all-helper p (make-u8vector 0) read_n_bytes u8vector-append bufsz)
   u8vector-length))

(define read_char read-char)
(define peek_char peek-char)

(define (read_n_chars n #!optional (p (current-input-port)))
  (check-array-for-eof 
   (read-n-helper p n make-string read-substring scm-substring)
   string-length))

(define (scm-read_all_chars #!optional (p (current-input-port)) (bufsz *def-buf-sz*))
  (check-array-for-eof 
   (read-all-helper p "" read_n_chars string-append bufsz)
   string-length))

(define read_all_chars scm-read_all_chars)

(define read_line read-line)
(define read_all read-all)

(define write_byte write-u8)

(define (write_bytes bytes #!optional (p (current-output-port)))
  (write-subu8vector bytes 0 (u8vector-length bytes) p))

(define (write_n_bytes bytes start end #!optional (p (current-output-port)))
  (write-subu8vector bytes start end p))

(define write_char write-char)

(define (write_chars str #!optional (p (current-output-port)))
  (write-substring str 0 (string-length str) p))

(define (write_n_chars str start end #!optional (p (current-output-port)))
  (write-substring str start end p))

(define (flush_writer #!optional (w (current-output-port)))
  (if (bits-writer-info? w)
      (bits-writer-flush w)
      (force-output w)))

(define (open-process-stream-helper path direction arguments environment
                                    directory stdin_redirection
                                    stdout_redirection stderr_redirection
                                    pseudo_terminal show_console
                                    tcoder)
  (let ((settings (scm-list path: path 
                        direction: direction
                        arguments: arguments 
                        environment: environment
                        directory: directory 
                        stdin-redirection: stdin_redirection
                        stdout-redirection: stdout_redirection 
                        stderr-redirection: stderr_redirection 
                        pseudo-terminal: pseudo_terminal
                        show-console: show_console)))
    (let ((opt (get-codec-from-transcoder tcoder)))
      (if opt (set! settings (scm-append settings (scm-list char-encoding: (replace-underscores opt))))))
    (let ((opt (get-error-handling-mode-from-transcoder tcoder)))
      (if opt (set! settings (scm-append settings (scm-list char-encoding-errors: opt)))))
    (let ((opt (get-eol-encoding-from-transcoder tcoder)))
      (if opt (set! settings (scm-append settings (scm-list eol-encoding: opt)))))
    (open-process settings)))

(define (open-pipe-stream path #!key (direction 'input-output) (arguments '()) environment
                          directory (stdin_redirection #t) (stdout_redirection #t)
                          stderr_redirection pseudo_terminal
                          show_console transcoder)
  (open-process-stream-helper path direction arguments environment directory stdin_redirection
                              stdout_redirection stderr_redirection pseudo_terminal show_console
                              transcoder))

(define pipe_process_pid process-pid)
(define pipe_process_status process-status)

(define (pipe_reader path #!key (arguments '()) environment
                     directory (stdin_redirection #t) (stdout_redirection #t)
                     stderr_redirection pseudo_terminal
                     show_console transcoder)
  (open-pipe-stream path direction: 'input arguments: arguments
                    environment: environment directory: directory
                    stdin_redirection: stdin_redirection
                    stdout_redirection: stdout_redirection
                    stderr_redirection: stderr_redirection
                    pseudo_terminal: pseudo_terminal
                    show_console: show_console
                    transcoder: transcoder))

(define (pipe_writer path #!key (arguments '()) environment
                     directory (stdin_redirection #t) (stdout_redirection #t)
                     stderr_redirection pseudo_terminal
                     show_console transcoder)
  (open-pipe-stream path direction: 'output arguments: arguments
                    environment: environment directory: directory
                    stdin_redirection: stdin_redirection
                    stdout_redirection: stdout_redirection
                    stderr_redirection: stderr_redirection
                    pseudo_terminal: pseudo_terminal
                    show_console: show_console
                    transcoder: transcoder))

(define (pipe_stream path #!key (arguments '()) environment
                     directory (stdin_redirection #t) (stdout_redirection #t)
                     stderr_redirection pseudo_terminal
                     show_console transcoder)
  (open-pipe-stream path direction: 'input-output arguments: arguments
                    environment: environment directory: directory
                    stdin_redirection: stdin_redirection
                    stdout_redirection: stdout_redirection
                    stderr_redirection: stderr_redirection
                    pseudo_terminal: pseudo_terminal
                    show_console: show_console
                    transcoder: transcoder))

(define (tcp_client_stream addr #!key port_number keep_alive (coalesce #t) transcoder)
  (let ((settings (scm-list)))
    (if (string? addr)
        (set! settings (scm-append settings (scm-list server-address: addr)))
        (set! port_number addr))
    (if (integer? port_number)
        (set! settings (scm-append settings (scm-list port-number: port_number))))
    (set! settings (scm-append settings (scm-list keep-alive: keep_alive coalesce: coalesce)))
    (let ((opt (get-codec-from-transcoder transcoder)))
      (if opt (set! settings (scm-append settings (scm-list char-encoding: (replace-underscores opt))))))
    (let ((opt (get-error-handling-mode-from-transcoder transcoder)))
      (if opt (set! settings (scm-append settings (scm-list char-encoding-errors: opt)))))
    (let ((opt (get-eol-encoding-from-transcoder transcoder)))
      (if opt (set! settings (scm-append settings (scm-list eol-encoding: opt)))))
    (open-tcp-client settings)))

(define (open-tcp-server-helper fn addr port_number backlog reuse_address transcoder #!optional cbfn)
  (let ((settings (scm-list)))
    (if (string? addr)
        (set! settings (scm-append settings (scm-list server-address: addr)))
        (set! port_number addr))
    (if (integer? port_number)
        (set! settings (scm-append settings (scm-list port-number: port_number))))
    (set! settings (scm-append settings (scm-list backlog: backlog reuse-address: reuse_address)))
    (let ((opt (get-codec-from-transcoder transcoder)))
      (if opt (set! settings (scm-append settings (scm-list char-encoding: (replace-underscores opt))))))
    (let ((opt (get-error-handling-mode-from-transcoder transcoder)))
      (if opt (set! settings (scm-append settings (scm-list char-encoding-errors: opt)))))
    (let ((opt (get-eol-encoding-from-transcoder transcoder)))
      (if opt (set! settings (scm-append settings (scm-list eol-encoding: opt)))))
    (if cbfn (fn settings cbfn)
        (fn settings))))

(define (tcp_server_stream addr #!key port_number (backlog 128) (reuse_address #t) transcoder)
  (open-tcp-server-helper open-tcp-server addr port_number backlog reuse_address transcoder))

(define (make-delimited-tokenizer stream delimiters)
  (let ((current-token #f))
    (lambda (msg)
      (case msg
        ((next)
         (if (scm-not current-token)
             (next-delimited-token stream delimiters)
             (let ((tmp current-token))
               (set! current-token #f)
               tmp)))
        ((peek)
         (if (scm-not current-token)
             (set! current-token (next-delimited-token stream delimiters)))
         current-token)
        (else (scm-error "invalid message received by delimited-tokenizer" msg))))))

(define (next-delimited-token stream delimiters)
  (if (eof-object? (peek-char stream))
      (read-char stream)
      (let ((cmpr (if (list? delimiters) memv char=?)))
        (let loop ((str '())
                   (c (peek-char stream)))
          (if (or (eof-object? c)
                  (cmpr c delimiters))
              (begin (read-char stream)
                     (list->string (scm-reverse str)))
              (loop (scm-cons (read-char stream) str) (peek-char stream)))))))

(define (stream_tokenizer delimiters #!optional (stream (current-input-port))) 
  (if delimiters (make-delimited-tokenizer stream delimiters)
      (make-tokenizer stream '())))

(define (peek_token tokenizer)
  (tokenizer 'peek))

(define (get_token tokenizer)
  (tokenizer 'next))

(define (scm-show #!key (stream (current-output-port)) (quotes #f) #!rest objs)
  (let loop ((objs objs))
    (if (scm-not (null? objs))
	(begin (slgn-display (scm-car objs) port: stream quotes: quotes)
	       (loop (scm-cdr objs))))))

(define show scm-show)

(define (scm-showln #!key (stream (current-output-port)) (quotes #f) #!rest objs)
  (let loop ((objs objs))
    (if (scm-not (null? objs))
	(begin (slgn-display (scm-car objs) port: stream quotes: quotes)
	       (loop (scm-cdr objs)))))
  (scm-newline stream))

(define showln scm-showln)

(define reader_timeout input-port-timeout-set!)
(define writer_timeout output-port-timeout-set!)

(define (print #!key (stream (current-output-port)) #!rest objs)
  (scm-apply scm-print port: stream objs))

(define (println #!key (stream (current-output-port)) #!rest objs)
  (scm-apply scm-println port: stream objs))

;; bit streams
(define-structure bits-reader-info input byte bit)
(define-structure bits-writer-info output byte bit)

(define (bits-stream? obj)
  (or (bits-reader-info? obj)
      (bits-writer-info? obj)))

(define (slgn-bits_reader input)
  (if (scm-not (input-port? input))
      (scm-error "object is not an input stream" input))
  (make-bits-reader-info input 0 0))

(define bits_reader slgn-bits_reader)

(define (bits_writer output)
  (if (scm-not (output-port? output))
      (scm-error "object is not an output stream" output))
  (make-bits-writer-info output 0 7))

(define (slgn-read_bit b)
  (let ((input (bits-reader-info-input b))
        (byte (bits-reader-info-byte b))
        (bit (bits-reader-info-bit b)))
    (cond ((scm-= 0 bit)
           (let ((byte (read-u8 input)))
             (cond ((eof-object? byte)
                    byte)
                   (else
                    (bits-reader-info-byte-set! b byte)
                    (bits-reader-info-bit-set! b 128)
                    (slgn-read_bit b)))))
          (else
           (bits-reader-info-bit-set! b (scm-quotient bit 2))
           (scm-> (bitwise-and byte bit) 0)))))

(define read_bit slgn-read_bit)

(define (bits_reader_align b)
  (bits-reader-info-bit-set! b 0))

(define (read_bits b n)
  (if (or (scm-<= n 0) (scm-> n 32))
      (scm-error "invalid argument" n))
  (let loop ((x (scm-- n 1))
             (r 0))
    (if (scm->= x 0)
        (let ((next (slgn-read_bit b)))
          (if (eof-object? next)
              next
              (loop (scm-- x 1) (bitwise-ior
                             r (arithmetic-shift
                                (if next 1 0)
                                x)))))
        r)))

(define (bits-writer-flush b)
  (if (scm-< (bits-writer-info-bit b) 7)
      (write-u8 (bits-writer-info-byte b)
                (bits-writer-info-output b)))
  (bits-writer-info-byte-set! b 0)
  (bits-writer-info-bit-set! b 7)
  *void*)

(define bits_writer_flush bits-writer-flush)

(define (slgn-write_bit b v)
  (let ((bit (bits-writer-info-bit b)))
    (cond ((scm-= bit -1)
           (bits-writer-flush b)
           (slgn-write_bit b v))
          (else
           (if (scm-not (scm-= v 0))
               (bits-writer-info-byte-set!
                b (bitwise-ior (bits-writer-info-byte b)
                               (arithmetic-shift 1 bit))))
           (bits-writer-info-bit-set! b (scm-- bit 1))
           *void*))))

(define write_bit slgn-write_bit)

(define (write_bits b v n)
  (let loop ((x (scm-- n 1)))
    (if (scm->= x 0)
        (begin (slgn-write_bit b (bitwise-and v (arithmetic-shift 1 x)))
               (loop (scm-- x 1))))))

