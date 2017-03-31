;; Copyright (C) 2013-2017 by Vijay Mathew Pandyalakal <vijay.the.lisper@gmail.com>

(define-structure %bitvector size vector)

(define %bitvect-bpi% 32)
(define %bitvect-shift% 5)
(define %bitvect-mask% #b00000000000000000000000000000001)
(define %bitvect-masks% (list->u32vector (let loop ((res '())
                                                    (i 0))
                                           (if (< i %bitvect-bpi%)
                                               (loop (scm-cons (arithmetic-shift %bitvect-mask% i) res)
                                                     (+ i 1))
                                               (scm-reverse res)))))
(define %bitvect-all-on% #b11111111111111111111111111111111)
(define %bitvect-all-off% #b00000000000000000000000000000000)

(define (%bitvector-vect-size bitvect-size)
  (+ (if (> (scm-remainder bitvect-size %bitvect-bpi%) 0) 1 0)
     (scm-quotient bitvect-size %bitvect-bpi%)))

(define (%bitvector-vector-length self) (u32vector-length (%bitvector-vector self)))

(define (make-bitvector size #!optional vector)
  (make-%bitvector size (if vector vector (make-u32vector (%bitvector-vect-size size)))))

(define (slgn-bit_array . rest)
  (let loop ((rest rest)
             (i 0)
             (ba (make-bitvector (scm-length rest))))
    (if (null? rest) ba
        (begin (if (scm-not (= 0 (scm-car rest)))
                   (bitvector-set! ba i))
               (loop (scm-cdr rest) (+ i 1) ba)))))

(define bit_array slgn-bit_array)

(define make_bit_array make-bitvector)

(define is_bit_array %bitvector?)

(define (bitvector-set! self i)
  (let ((vec (%bitvector-vector self))
        (idx (fxwraplogical-shift-right i %bitvect-shift%)))
    (u32vector-set! vec idx (bitwise-ior (u32vector-ref %bitvect-masks% (- i (* %bitvect-bpi% idx)))
                                         (u32vector-ref vec idx)))))

(define bit_array_set bitvector-set!)

(define (bitvector-set? self i)
  (let ((vec (%bitvector-vector self))
        (idx (fxwraplogical-shift-right i %bitvect-shift%)))
    (scm-not (zero? (bitwise-and (u32vector-ref %bitvect-masks% (- i (* %bitvect-bpi% idx)))
                             (u32vector-ref vec idx))))))

(define bit_array_is_set bitvector-set?)

(define (bitvector-clear! self i)
  (let ((vec (%bitvector-vector self))
        (idx (fxwraplogical-shift-right i %bitvect-shift%)))
    (u32vector-set! vec idx (bitwise-and (bitwise-not (u32vector-ref %bitvect-masks% (- i (* %bitvect-bpi% idx))))
                                         (u32vector-ref vec idx)))))

(define bit_array_clear bitvector-clear!)

(define (bitvector-length self) (%bitvector-size self))

(define bit_array_length bitvector-length)

(define (%bitvector-i-at self index) (if (bitvector-set? self index) 1 0))

(define (%bitvector-set-all! self v)
  (let ((vect (%bitvector-vector self)))
    (let loop ((i (- (%bitvector-vector-length self) 1)))
      (cond ((>= i 0)
             (u32vector-set! vect i v)
             (loop (- i 1)))))))

(define (bit_array_set_all self) (%bitvector-set-all! self %bitvect-all-on%))
(define (bit_array_clear_all self) (%bitvector-set-all! self %bitvect-all-off%))

(define (bitvector-all-set? self)
  (let ((len (%bitvector-vector-length self))
        (v (%bitvector-vector self)))
    (let loop ((res #t)
               (i 0))
      (if (and res (< i len))
          (loop (= %bitvect-all-on% (u32vector-ref v i)) (+ i 1))
          res))))

(define bit_array_is_all_set bitvector-all-set?)

(define (bitvector-one-set? self)
  (let ((len (%bitvector-vector-length self))
        (v (%bitvector-vector self)))
    (let loop ((res #t)
               (i 0))
      (if (and res (< i len))
          (loop (= %bitvect-all-off% (u32vector-ref v i)) (+ i 1))
          (scm-not res)))))

(define bit_array_is_any_set bitvector-one-set?)

(define (bitvector-for-each fn self)
  (let ((size (%bitvector-size self)))
    (let loop ((i 0))
      (cond ((< i size)
             (fn (%bitvector-i-at self i))
             (loop (+ i 1)))))))

(define (bitvector->list self)
  (let ((res '()))
    (bitvector-for-each 
     (lambda (i)
       (set! res (scm-cons i res)))
     self)
    (scm-reverse res)))

(define bit_array_to_list bitvector->list)

(define (list->bitvector lst) (scm-apply bit_array lst))

(define list_to_bit_array list->bitvector)

(define (string->bitvector s)
  (let ((b (make-bitvector (string-length s)))
        (i 0))
    (for-each
     (lambda (c)
       (cond ((char=? c #\1)
              (bitvector-set! b i))
             (else
              (if (scm-not (char=? c #\0))
                  (scm-error "not a valid bitstring"))))
       (set! i (+ i 1)))
     (string->list s))
    b))

(define (bitvector->string self)
  (let ((s (make-string (%bitvector-size self) #\0))
        (i 0))
    (bitvector-for-each
     (lambda (b)
       (if (= b 1)
           (string-set! s i #\1))
       (set! i (+ i 1)))
     self)
    s))

(define bit_array_to_string bitvector->string)
(define string_to_bit_array string->bitvector)

(define (bitvector=? self that)
  (let ((slen (%bitvector-vector-length self))
        (tlen (%bitvector-vector-length that)))
    (if (= slen tlen)
        (let ((svec (%bitvector-vector self))
              (tvec (%bitvector-vector that)))
          (let loop ((i 0)
                     (res #t))
            (if (and res (< i slen))
                (loop (+ i 1) (= (u32vector-ref svec i)
                                 (u32vector-ref tvec i)))
                res)))
        #f)))

(define bit_array_is_eq bitvector=?)

;; TODO: Optimize this.
(define (bitvector-blit! self offset target target-offset count)
  (let loop ((si offset)
             (ti target-offset)
             (i 0))
    (if (< i count)
        (begin
          (if (bitvector-set? self si)
              (bitvector-set! target ti)
              (bitvector-clear! target ti))
          (loop (+ si 1)
                (+ ti 1)
                (+ i 1)))))
  target)

(define bit_array_blit bitvector-blit!)

(define (bitvector-fold-left fn self x)
  (let ((len (bitvector-length self)))
    (let loop ((i 0)
               (res x))
      (if (< i len)
          (loop (+ i 1) (fn res (%bitvector-i-at self i)))
          res))))

(define (bitvector-fold-right fn self x)
  (let ((len (bitvector-length self)))
    (let loop ((i (- len 1))
               (res x))
      (if (>= i 0)
          (loop (- i 1) (fn res (%bitvector-i-at self i)))
          res))))

(define (bit-array-reduce self fn #!key (initial_value 0) from_right)
  ((if from_right bitvector-fold-right bitvector-fold-left)
   fn self initial_value))

(define (bit-array-append a b)
  (let ((lena (bitvector-length a))
        (lenb (bitvector-length b)))
    (let ((self (make-bitvector (+ lena lenb))))
      (bitvector-blit! a 0 self 0 lena)
      (bitvector-blit! b 0 self lena lenb))))

(define (bit_arrays_concat bitvect-list)
  (if (= 1 (scm-length bitvect-list))
      (scm-car bitvect-list)
      (let loop ((self (scm-car bitvect-list))
                 (bitvect-list (scm-cdr bitvect-list)))
        (if (scm-not (null? bitvect-list))
            (loop (bit-array-append self (scm-car bitvect-list))
                  (scm-cdr bitvect-list))
            self))))

(define (subbitarray self offset count)
  (bitvector-blit! self offset (make-bitvector (- count offset)) 0 count))

(define scm-subbitarray subbitarray)

(define *u8array-pad* (make-u8vector 1))

(define (pad-u8array u8arr len)
  (let loop ((u8arr u8arr) (len len))
    (if (scm-not (zero? (scm-mod len 4)))
        (loop (u8vector-append u8arr *u8array-pad*)
              (+ len 1))
        u8arr)))

(define (pack-u8->u32 u8arr offset len)
  (let loop ((i 0) (offset offset) (n 0))
    (cond ((< i 4)
           (set! n (arithmetic-shift n 8))
           (loop (+ i 1) (+ offset 1)
                 (bitwise-ior n (bitwise-and (u8vector-ref u8arr offset) #xFF))))
          (else n))))

(define (u8array_to_bit_array u8arr)
  (let ((bits-stream (slgn-bits_reader (slgn-byte_array_reader u8arr))))
    (let loop ((b (slgn-read_bit bits-stream))
               (bits '()))
      (if (eof-object? b)
          (scm-apply slgn-bit_array (scm-reverse bits))
          (loop (slgn-read_bit bits-stream) (scm-cons (if b 1 0) bits))))))

(define (bitvector-ref a i)
  (if (bitvector-set? a i) 1 0))

(define bit_array_at bitvector-ref)
