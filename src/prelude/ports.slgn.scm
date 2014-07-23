;; Copyright (c) 2013-2014 by Vijay Mathew Pandyalakal, All Rights Reserved.

(define-structure text-transcoder codec eol-style error-handling-mode)

(define *supported-codecs* '(ISO-8859-1 
                             ASCII UTF-8 UTF-16 UTF-16LE
                             UTF-16BE UCS-2 UCS-2LE UCS-2BE UCS-4 UCS-4LE UCS-4BE))

(define (validate-codec codec)
  (if (not (symbol? codec))
      (error "Codec must be a symbol."))
  (if (not (memq (string->symbol (string_upcase (symbol->string codec)))
                 *supported-codecs*))
      (error "Unsupported codec." codec))
  codec)

(define (validate-error-handling-mode mode)
  (if (or (eq? mode 'replace) (eq? mode 'raise)) mode
      (error "Invalid error handling mode." mode)))

(define *eol-styles* '(lf cr crlf))

(define (validate-eol-style eol-style)
  (if (memq eol-style *eol-styles*) eol-style
      (error "Unsupported eol-style." eol-style)))

(define (error-handling-mode->boolean mode) (eq? mode 'replace))

(define (eol-style->encoding eol-style)
  (if (eq? eol-style 'crlf) 'cr-lf
      eol-style))

(define (transcoder codec #!optional (eol-style 'crlf) (error-handling-mode #t))
  (make-text-transcoder (validate-codec codec) (validate-eol-style eol-style) 
                        (validate-error-handling-mode error-handling-mode)))

(define (trancoder_codec t) (text-transcoder-codec t))
(define (transcoder_eol_style t) (text-transcoder-eol-style t))
(define (transcoder_error_handling_mode t) (text-transcoder-error-handling-mode t))

(define (get-port-option options key #!optional default)
  (if (list? options)
      (if (memq key options) #t
          default)
      default))

(define (get-codec-from-transcoder tcoder)
  (if tcoder (text-transcoder-codec tcoder)
      #f))

(define (get-error-handling-mode-from-transcoder tcoder)
  (if tcoder (error-handling-mode->boolean (text-transcoder-error-handling-mode tcoder))
      #f))

(define (get-eol-encoding-from-transcoder tcoder)
  (if tcoder (eol-style->encoding (text-transcoder-eol-style tcoder))
      'cr-lf))

(define (get-buffering-for-port bmode)
  (if bmode
      (case bmode
        ((block) #t)
        ((none) #f)
        ((line) 'line)
        (else (error "Invalid buffer-mode." bmode)))
      bmode))

(define (open-file-port-helper path direction options buffer-mode tcoder permissions)
  (let ((settings (list path: path direction: direction)))
    (let ((opt (get-port-option options 'append)))
      (if opt (set! settings (append settings (list append: opt)))))
    (if (not (eq? direction 'input))
        (let ((opt (get-port-option options 'create 'maybe)))
          (if opt (set! settings (append settings (list create: opt))))))
    (let ((opt (get-port-option options 'truncate)))
      (if opt (set! settings (append settings (list truncate: opt)))))
    (let ((opt (get-codec-from-transcoder tcoder)))
      (if opt (set! settings (append settings (list char-encoding: opt)))))
    (let ((opt (get-error-handling-mode-from-transcoder tcoder)))
      (if opt (set! settings (append settings (list char-encoding-errors: opt)))))
    (let ((opt (get-eol-encoding-from-transcoder tcoder)))
      (if opt (set! settings (append settings (list eol-encoding: opt)))))
    (if permissions (set! settings (append settings (list permissions: permissions))))
    (open-file (append settings (list buffering: (get-buffering-for-port buffer-mode))))))

(define (open_file_input_port path #!optional options buffer-mode tcoder)
  (open-file-port-helper path 'input options buffer-mode tcoder #f))

(define (open_file_output_port path #!optional options buffer-mode tcoder (permissions #o666))
  (open-file-port-helper path 'output options buffer-mode tcoder permissions))

(define (open_file_input_output_port path #!optional options buffer-mode tcoder (permissions #o666))
  (open-file-port-helper path 'input-output options buffer-mode tcoder permissions))

(define open_input_file open-input-file)
(define open_output_file open-output-file)

(define current_input_port current-input-port)
(define current_output_port current-output-port)
(define current_error_port current-error-port)

(define (open-byte-port-helper openfn invalue tcoder)
  (if (not tcoder)
      (openfn invalue)
      (let ((settings (list init: invalue)))
        (let ((opt (get-codec-from-transcoder tcoder)))
          (if opt (set! settings (append settings (list char-encoding: opt)))))
        (let ((opt (get-error-handling-mode-from-transcoder tcoder)))
          (if opt (set! settings (append settings (list char-encoding-errors: opt)))))
        (let ((opt (get-eol-encoding-from-transcoder tcoder)))
          (if opt (set! settings (append settings (list eol-encoding: opt)))))
        (openfn settings))))

(define (open_byte_array_input_port #!optional byte_array tcoder)
  (open-byte-port-helper 
   open-input-u8vector 
   (if byte_array byte_array (make-u8vector 0)) 
   tcoder))

(define (open_byte_array_output_port #!optional byte_array tcoder)
  (open-byte-port-helper 
   open-output-u8vector 
   (if byte_array byte_array (make-u8vector 0)) 
   tcoder))

(define (open_string_input_port #!optional str)
  (open-byte-port-helper 
   open-input-string 
   (if str str (make-string 0)) 
   #f))

(define (open_string_output_port #!optional str)
  (open-byte-port-helper
   open-output-string
   (if str str (make-string 0))
   #f))

(define get_output_string get-output-string)

(define is_port port?)
(define is_input_port input-port?)
(define is_output_port output-port?)
(define close_port close-port)
(define close_input_port close-input-port)
(define close_output_port close-output-port)

(define (port_position p)
  (if (input-port? p)
      (input-port-byte-position p)
      (output-port-byte-position p)))

(define (port_has_port_position p)
  (with-exception-catcher 
   (lambda (e) #f)
   (lambda () (if (port_position p) #t #f))))

(define (whence->integer w)
  (case w
    ((beginning) 0)
    ((current) 1)
    ((end) 2)
    (else (error "Invalid whence for port position change." w))))

(define (set_port_position p pos #!optional (whence 'beginning))
  (if (input-port? p)
      (input-port-byte-position p pos (whence->integer whence))
      (output-port-byte-position p pos (whence->integer whence))))

(define (safely-close-port p)
  (if (port? p)
      (with-exception-catcher
       (lambda (e) #f)
       (lambda () (close-port p)))))

(define (call_with_port p proc)
  (with-exception-catcher
   (lambda (e)
     (safely-close-port p)
     (raise e))
   (lambda () 
     (let ((r (proc p))) 
       (safely-close-port p)
       r))))

(define (eof_object) #!eof)
(define is_eof_object eof-object?)

(define *def-buf-sz* 1024)

(define read_byte read-u8)

(define (read-n-helper p n initfn rdfn subfn)
  (let ((r (initfn n)))
    (let ((n (rdfn r 0 n p n)))
      (if (zero? n) #f
          (subfn r 0 n)))))
  
(define (read_byte_n p n)
  (read-n-helper p n make-u8vector read-subu8vector subu8vector))

(define (read-all-helper p init rdfn apndfn bufsz)
  (let loop ((r init)
             (nr (rdfn p bufsz)))
    (if (not nr) r
        (loop (apndfn r nr)
              (rdfn p bufsz)))))

(define (read_byte_all p #!optional (bufsz *def-buf-sz*))
  (read-all-helper p (make-u8vector 0) read_byte_n u8vector-append bufsz))

(define read_char read-char)
(define peek_char peek-char)

(define (read_char_n p n)
  (read-n-helper p n make-string read-substring substring))

(define (read_char_all p #!optional (bufsz *def-buf-sz*))
  (read-all-helper p "" read_char_n string-append bufsz))

(define read_line read-line)
(define read_all read-all)

(define (read_all_lines #!optional (port (current-input-port)) #!key as_list)
  (let ((chars (read-all port read-char)))
    (if (list? chars) 
        (let ((r (list->string chars)))
          (if as_list (string_split r #\newline) r))
        chars)))

(define (read_all_bytes #!optional (port (current-input-port)))
  (read-all port read_byte))

(define write_byte write-u8)
(define (write_byte_n p bytes)
  (write-subu8vector bytes 0 (u8vector-length bytes) p))

(define write_char write-char)
(define (write_char_n p str)
  (write-substring str 0 (string-length str) p))

(define flush_output_port force-output)

(define file_exists file-exists?)
(define delete_file delete-file)

(define (open-process-port-helper path direction arguments environment
                                  directory stdin_redirection
                                  stdout_redirection stderr_redirection
                                  pseudo_terminal show_console
                                  tcoder)
  (let ((settings (list path: path 
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
      (if opt (set! settings (append settings (list char-encoding: opt)))))
    (let ((opt (get-error-handling-mode-from-transcoder tcoder)))
      (if opt (set! settings (append settings (list char-encoding-errors: opt)))))
    (let ((opt (get-eol-encoding-from-transcoder tcoder)))
      (if opt (set! settings (append settings (list eol-encoding: opt)))))
    (open-process settings)))

(define (open_process_output_port path #!key (arguments '()) environment
                                  directory stdin_redirection stdout_redirection
                                  stderr_redirection pseudo_terminal
                                  show_console transcoder)
  (open-process-port-helper path 'output arguments environment directory stdin_redirection
                            stdout_redirection stderr_redirection pseudo_terminal show_console
                            transcoder))

(define (open_process_input_port path #!key (arguments '()) environment
                                 directory stdin_redirection stdout_redirection
                                 stderr_redirection pseudo_terminal
                                 show_console transcoder)
  (open-process-port-helper path 'input arguments environment directory stdin_redirection
                            stdout_redirection stderr_redirection pseudo_terminal show_console
                            transcoder))

(define process_pid process-pid)
(define process_status process-status)

(define (open_tcp_client_port addr #!key port_number keep_alive (coalesce #t) transcoder)
  (let ((settings (list)))
    (if (string? addr)
        (set! settings (append settings (list server-address: addr)))
        (set! port_number addr))
    (if (integer? port_number)
        (set! settings (append settings (list port-number: port_number))))
    (set! settings (append settings (list keep-alive: keep_alive coalesce: coalesce)))
    (let ((opt (get-codec-from-transcoder transcoder)))
      (if opt (set! settings (append settings (list char-encoding: opt)))))
    (let ((opt (get-error-handling-mode-from-transcoder transcoder)))
      (if opt (set! settings (append settings (list char-encoding-errors: opt)))))
    (let ((opt (get-eol-encoding-from-transcoder transcoder)))
      (if opt (set! settings (append settings (list eol-encoding: opt)))))
    (open-tcp-client settings)))

(define (open-tcp-server-helper fn addr port_number backlog reuse_address transcoder #!optional cbfn)
  (let ((settings (list)))
    (if (string? addr)
        (set! settings (append settings (list server-address: addr)))
        (set! port_number addr))
    (if (integer? port_number)
        (set! settings (append settings (list port-number: port_number))))
    (set! settings (append settings (list backlog: backlog reuse-address: reuse_address)))
    (let ((opt (get-codec-from-transcoder transcoder)))
      (if opt (set! settings (append settings (list char-encoding: opt)))))
    (let ((opt (get-error-handling-mode-from-transcoder transcoder)))
      (if opt (set! settings (append settings (list char-encoding-errors: opt)))))
    (let ((opt (get-eol-encoding-from-transcoder transcoder)))
      (if opt (set! settings (append settings (list eol-encoding: opt)))))
    (if cbfn (fn settings cbfn)
        (fn settings))))

(define (open_tcp_server_port addr #!key port_number (backlog 128) (reuse_address #t) transcoder)
  (open-tcp-server-helper open-tcp-server addr port_number backlog reuse_address transcoder))

(define (make-delimited-tokenizer port delimiters)
  (let ((current-token #f))
    (lambda (msg)
      (case msg
        ((next)
         (if (not current-token)
             (next-delimited-token port delimiters)
             (let ((tmp current-token))
               (set! current-token #f)
               tmp)))
        ((peek)
         (if (not current-token)
             (set! current-token (next-delimited-token port delimiters)))
         current-token)
        (else (error "Invalid message received by delimited-tokenizer. " msg))))))

(define (next-delimited-token port delimiters)
  (if (eof-object? (peek-char port))
      (read-char port)
      (let ((cmpr (if (list? delimiters) memv char=?)))
        (let loop ((str '())
                   (c (peek-char port)))
          (if (or (eof-object? c)
                  (cmpr c delimiters))
              (begin (read-char port)
                     (list->string (reverse str)))
              (loop (cons (read-char port) str) (peek-char port)))))))

(define (port_tokenizer port #!key with_delimiters) 
  (if with_delimiters (make-delimited-tokenizer port with_delimiters)
      (make-tokenizer port '())))

(define (peek_token tokenizer)
  (tokenizer 'peek))

(define (get_token tokenizer)
  (tokenizer 'next))

(define (get_datum tokenizer)
  (slogan tokenizer))

(define (write_datum obj #!optional (to (current-output-port)))
  (slgn-display obj port: to))


(define (list_directory dirname #!key ignore_hidden)
  (let ((d (open-directory (list path: dirname ignore-hidden: ignore_hidden))))
    (let ((ls (read-all d)))
      (close-input-port d)
      ls)))

(define (create_directory path #!optional (permissions #o777))
  (create-directory (list path: path permissions: permissions)))

(define delete_directory delete-directory)

(define create_link create-link)
(define create_symbolic_link create-symbolic-link)
(define rename_file rename-file)
(define copy_file copy-file)
(define delete_file delete-file)

(define path_expand path-expand)
(define path_normalize path-normalize)

(define path_extension path-extension)
(define path_strip_extension path-strip-extension)
(define path_directory path-directory)
(define path_strip_directory path-strip-directory)
(define path_strip_trailing_directory_separator path-strip-trailing-directory-separator)
(define path_volume path-volume)
(define path_strip_volume path-strip-volume)

(define system shell-command)

(define (show #!key (port (current-output-port)) #!rest objs)
  (let loop ((objs objs))
    (if (not (null? objs))
	(begin (slgn-display (car objs) display-string: #t port: port)
	       (loop (cdr objs))))))

(define (showln #!key (port (current-output-port)) #!rest objs)
  (let loop ((objs objs))
    (if (not (null? objs))
	(begin (slgn-display (car objs) display-string: #t port: port)
	       (loop (cdr objs)))))
  (newline port))
