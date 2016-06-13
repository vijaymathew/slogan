;; Copyright (c) 2013-2016 by Vijay Mathew Pandyalakal, All Rights Reserved.

(define (scm-repr->slgn-repr val)
  (if (or (number? val) 
          (string? val)
          (char? val)) 
      val
      (repr-convert val *scheme-reprs*)))

(define (slgn-repr->scm-repr val)
  (repr-convert val *slogan-reprs*))

(define (slgn-directive->scm-directive s)
  (case s
    ((@optional) '#!optional)
    ((@key) '#!key)
    ((@rest) '#!rest)
    (else s)))

(define (slgn-variable->scm-keyword var)
  (string->keyword (symbol->string var)))

(define (slgn-symbol->scm-sym/kw s convfn)
  (let ((str (symbol->string s)))
    (convfn
     (string_replace_all 
      str
      #\_ #\-))))

(define (scm-symbol->slgn-sym s)
  (let ((str (symbol->string s)))
    (string->symbol
     (string_replace_all 
      str
      #\- #\_))))

(define (slgn-symbol->scm-keyword s)
  (slgn-symbol->scm-sym/kw s string->keyword))

(define (slgn-symbol-quote? s)
  (and (list? s) (scm-not (null? s))
       (scm-eq? (scm-car s) 'quote)))

(define (slgn-path/settings->scm-path/settings path-or-settings)
  (if (string? path-or-settings)
      path-or-settings
      (let loop ((settings path-or-settings)
                 (result '()))
        (if (null? settings)
            result
            (loop (scm-cdr settings)
                  (scm-append (scm-list (slgn-symbol->scm-keyword (scm-car (scm-car settings)))
                                (scm-cdr (scm-car settings)))
                          result))))))

(define (repr-convert val reprs)
  (let ((r (scm-assq val reprs)))
    (if r (scm-cdr r) val)))

(define (void? val) (scm-eq? *void* val))

(define (slgn-display val #!key (port (current-output-port)))
  (if (scm-not (void? val))
      (cond ((procedure? val)
             (slgn-display-function port))
            ((set? val)
             (slgn-display-set val port))            
            ((list? val)
             (slgn-display-list val port))
            ((pair? val)
             (slgn-display-pair val port))
            ((string? val)
             (scm-write val port))
            ((char? val)
             (slgn-display-char val port))
            ((vector? val)
             (slgn-display-array val port "#" vector->list))
            ((hashtable? val)
             (slgn-display-hashtable val port))
            ((u8vector? val)
             (slgn-display-array val port "#u8" u8vector->list))
            ((s8vector? val)
             (slgn-display-array val port "#s8" s8vector->list))
            ((s16vector? val)
             (slgn-display-array val port "#s16" s16vector->list))
            ((u16vector? val)
             (slgn-display-array val port "#u16" u16vector->list))            
            ((s32vector? val)
             (slgn-display-array val port "#s32" s32vector->list))
            ((u32vector? val)
             (slgn-display-array val port "#u32" u32vector->list))            
            ((s64vector? val)
             (slgn-display-array val port "#s64" s64vector->list))
            ((u64vector? val)
             (slgn-display-array val port "#u64" u64vector->list))            
            ((f32vector? val)
             (slgn-display-array val port "#f32" f32vector->list))
            ((f64vector? val)
             (slgn-display-array val port "#f64" f64vector->list))            
            ((%bitvector? val)
             (slgn-display-array val port "#b" bitvector->list))
	    ((thread? val)
	     (slgn-display-task port))
            ((error-exception? val)
             (display-exception val port))
            ((eof-object? val)
             (scm-display '<eof> port))
            ((reactive-var? val)
             (slgn-display-rvar port))
            ((##promise? val)
             (slgn-display-promise port))
            ((s-yield? val)
             (slgn-display-special-obj "iterator" port))
            ((condition-variable? val)
             (slgn-display-special-obj "monitor" port))
            (else
             (scm-display (scm-repr->slgn-repr val) port)))))

(define (slgn-display-hashtable ht port)
  (scm-display "#{" port)
  (let ((t (hashtable-table ht)))
    (let ((len (- (table-length t) 1))
          (i 0))
      (table-for-each (lambda (k v)
                        (slgn-display k port: port)
                        (scm-display ": " port)
                        (slgn-display v port: port)
                        (set! i (+ 1 i))
                        (if (not (> i len))
                            (begin
                              (if *sep-char* (scm-display *sep-char* port))
                              (scm-display #\space port))))
                      t)))
  (scm-display #\} port))

(define (slgn-display-set s port)
  (scm-display "#(" port)
  (let loop ((xs (scm-reverse (set->list s))))
    (if (not (null? xs))
        (begin (slgn-display (scm-car xs) port: port)
               (if (not (null? (scm-cdr xs)))
                   (scm-display #\, port))
               (loop (scm-cdr xs)))))
  (scm-display #\) port))
    
(define (slgn-display-special-obj tag port)
  (scm-display "<" port) (scm-display tag port)
  (scm-display ">" port))
  
(define (slgn-display-rvar port) 
  (slgn-display-special-obj "rvar" port))

(define (slgn-display-function port)
  (slgn-display-special-obj "function" port))

(define (slgn-display-promise port)
  (slgn-display-special-obj "promise" port))

(define *sep-char* #\,)

(define (show_comma_separator flag) 
  (if flag
      (set! *sep-char* #\,)
      (set! *sep-char* #f)))

(define (slgn-display-list lst port)
  (scm-display "[" port)
  (let loop ((lst lst))
    (cond ((null? lst)
           (scm-display "]" port))
          (else
           (slgn-display (scm-car lst) port: port)
           (if (scm-not (null? (scm-cdr lst)))
               (begin (if *sep-char* (scm-display *sep-char* port))
                      (scm-display #\space port)))
           (loop (scm-cdr lst))))))

(define (slgn-display-pair p port)
  (slgn-display (scm-car p) port: port)
  (scm-display ":" port)
  (slgn-display (scm-cdr p) port: port))

(define (slgn-display-array a port 
                            prefix tolist)
  (scm-display prefix port)
  (slgn-display-list (tolist a) port))

(define *special-chars* '(#\newline #\space #\tab #\return
                          #\backspace #\alarm #\vtab #\esc
                          #\delete #\nul))

(define (slgn-display-char c port)
  (scm-display #\\ port)  
  (with-exception-catcher
   (lambda (ex)
     (let* ((s (with-output-to-string
                '()
                (lambda () (scm-write c))))
            (len (string-length s))
            (subsi (if (> len 2)
                       (if (char=? #\# (string-ref s 0))
                           (if (char=? #\\ (string-ref s 1))
                               2
                               1)
                           #f)
                       #f)))
       (if subsi
           (scm-display (scm-substring s subsi len) port)
           (scm-display s port))))
   (lambda ()
     (if (scm-member c *special-chars*)
         (scm-display (let ((s (with-output-to-string '() (lambda () (scm-write c)))))
                        (scm-substring s 2 (string-length s)))
                      port)
         (scm-display (scm-string c) port)))))

(define (slgn-display-task port)
  (slgn-display-special-obj "task" port))

(define (generic-map1! f result vec len reff setf)
  (let loop ((i 0))
    (if (>= i len) 
        (if result result *void*)
        (let ((res (f (reff vec i))))
          (if result (setf result i res))
          (loop (+ i 1))))))

(define (generic-map2+! f result vecs len reff setf)
  (let loop ((i 0))
    (if (>= i len) 
        (if result result *void*)
        (let ((res (scm-apply f (reff vecs i)))) 
          (if result (setf result i res))
          (loop (+ i 1))))))

(define (assert-equal-lengths seq rest #!optional (lenf length))
  (let ((len (lenf seq)))
    (let loop ((rest rest))
      (if (scm-not (null? rest))
          (begin (if (scm-not (scm-eq? (lenf (scm-car rest)) len))
                     (error (with-output-to-string 
                              "Object is not of proper length: " 
                              (lambda () (slgn-display (scm-car rest))))))
                 (loop (scm-cdr rest)))))))

(define (has-envvars? path)
  (let ((len (string-length path)))
    (let loop ((i 0))
      (if (< i len)
          (if (char=? (string-ref path i) #\$)
              #t
              (loop (+ i 1)))
          #f))))

(define (extract-envvar path offset len)
  (let ((buff (open-output-string)))
    (let loop ((i offset))
      (if (< i len)
          (let ((c (string-ref path i)))
            (if (scm-not (or (char=? c #\/) (char=? c #\\)))
                (begin (write-char c buff)
                       (loop (+ i 1)))))))
    (get-output-string buff)))
                
(define (expand_envvars path)
  (let ((len (string-length path))
        (buff (open-output-string)))
    (let loop ((i 0))
      (if (< i len)
          (let ((c (string-ref path i)))
            (if (char=? c #\$)
                (let ((evar (extract-envvar path (+ i 1) len)))
                  (if evar
                      (begin (scm-display (getenv evar) buff)
                             (loop (+ i 1 (string-length evar))))
                      (loop (+ i 1))))
                (begin (write-char c buff)
                       (loop (+ i 1)))))
          (get-output-string buff)))))

(define (add-slgn-extn file-name)
  (if (string=? (path-extension file-name) *slgn-extn*)
      file-name
      (string-append file-name *slgn-extn*)))

(define (load script #!optional force-compile)
  (if force-compile
      (with-exception-catcher
       (lambda (e)
         (if (scm-not (no-such-file-or-directory-exception? e))
             (raise e)))
       (lambda ()
         (delete-file (string-append script *scm-extn*)))))
  (with-exception-catcher
   (lambda (e)
     (if (file-exists? (add-slgn-extn script))
         (if (compile script assemble: #f)
             (scm-load (string-append script *scm-extn*))
             (error "failed to compile script" script))
         (error "file not found " script)))
   (lambda () (scm-load script))))

(define (reload script) (load script #t))

(define (link script)
  (with-exception-catcher
   (lambda (e)
     (cond ((no-such-file-or-directory-exception? e)
            (if (file-exists? (add-slgn-extn script))
                (if (scm-not (compile script assemble: #t))
                    (error "failed to compile script" script)))
            (scm-load script))
           (else (raise e))))
   (lambda () (scm-load script))))

(define (check-for-? name tokenizer)
  (let ((name (if (pair? name) (cadr name) name)))
    (if (>= (string-indexof (symbol->string name) #\?) 0)
        (parser-error tokenizer "Invalid character `?` in variable name."))))

(define (sanitize-expression tokenizer expr)
  (if (and (list? expr) (scm-not (null? expr)))
      (let ((f (scm-car expr)))
        (cond ((or (eq? f 'define)
                   (eq? f 'set!))
               (check-for-? (scm-cadr expr) tokenizer))
              ((or (eq? f 'let)
                   (eq? f 'let*)
                   (eq? f 'letrec))
               (let ((e (if (symbol? (scm-cadr expr)) (scm-caddr expr) (scm-cadr expr))))
                 (scm-map (lambda (d) (check-for-? (scm-car d) tokenizer)) e))))))
  expr)

(define (display-all x . xs)
  (scm-display x)
  (let loop ((xs xs))
    (if (scm-not (null? xs))
        (begin
          (scm-display (scm-car xs))
          (loop (scm-cdr xs))))))
