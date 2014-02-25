;Copyright (C) 2013 by Vijay Mathew Pandyalakal <vijay.the.lisper@gmail.com>

(define-structure %bitvector size vector)

(define %bitvect-bpi% 32)
(define %bitvect-shift% 5)
(define %bitvect-mask% #b00000000000000000000000000000001)
(define %bitvect-masks% (list->u32vector (let loop ((res '())
                                                    (i 0))
                                           (if (< i %bitvect-bpi%)
                                               (loop (cons (arithmetic-shift %bitvect-mask% i) res)
                                                     (+ i 1))
                                               (reverse res)))))
(define %bitvect-all-on% #b11111111111111111111111111111111)
(define %bitvect-all-off% #b00000000000000000000000000000000)

(define (%bitvector-vect-size bitvect-size)
  (+ (if (> (remainder bitvect-size %bitvect-bpi%) 0) 1 0) 
     (quotient bitvect-size %bitvect-bpi%)))

(define (%bitvector-vector-length self) (u32vector-length (%bitvector-vector self)))

(define (make-bitvector size #!optional vector)
  (make-%bitvector size (if vector vector (make-u32vector (%bitvector-vect-size size)))))

(define bit_array make-bitvector)

(define is_bit_array u32vector?)

(define (bitvector-set! self i)
  (let ((vec (%bitvector-vector self))
        (idx (fxwraplogical-shift-right i %bitvect-shift%)))
    (u32vector-set! vec idx (bitwise-ior (u32vector-ref %bitvect-masks% (- i (* %bitvect-bpi% idx)))
                                         (u32vector-ref vec idx)))))

(define bit_array_set bitvector-set!)

(define (bitvector-set? self i)
  (let ((vec (%bitvector-vector self))
        (idx (fxwraplogical-shift-right i %bitvect-shift%)))
    (not (zero? (bitwise-and (u32vector-ref %bitvect-masks% (- i (* %bitvect-bpi% idx)))
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

(define (bitvector-for-each fn self)
  (let ((size (%bitvector-size self)))
    (let loop ((i 0))
      (cond ((< i size)
             (fn (%bitvector-i-at self i))
             (loop (+ i 1)))))))

(define bit_array_for_each bitvector-for-each)

(define (bitvector-for-each-i fn self)
  (let ((size (%bitvector-size self)))
    (let loop ((i 0))
      (cond ((< i size)
             (fn i (%bitvector-i-at self i))
             (loop (+ i 1)))))))

(define (bitvector-map fn bv01 #!rest bvs)
  (list->bitvector (if bvs
                       (apply map (cons fn (cons (bitvector->list bv01) 
                                                 (map bitvector->list bvs))))
                       (map fn (bitvector->list bv01)))))

(define bit_array_map bitvector-map)

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
          (not res)))))

(define bit_array_is_any_set bitvector-one-set?)

(define (bitvector->list self)
  (let ((res '()))
    (bitvector-for-each 
     (lambda (i)
       (set! res (cons i res)))
     self)
    (reverse res)))

(define bit_array_to_list bitvector->list)

(define (list->bitvector lst)
  (let ((self (make-bitvector (length lst))))
    (let loop ((lst lst) (i 0))
      (cond ((not (null? lst))
             (if (= (car lst) 1)
                 (bitvector-set! self i)
                 (if (not (zero? (car lst)))
                     (error "List element should be either 1 or 0.")))
             (loop (cdr lst) (+ i 1)))))
    self))

(define list_to_bit_array list->bitvector)

(define (string->bitvector s)
  (let ((b (make-bitvector (string-length s)))
        (i 0))
    (for-each
     (lambda (c)
       (cond ((char=? c #\1)
              (bitvector-set! b i))
             (else
              (if (not (char=? c #\0))
                  (error "Not a valid bitstring."))))
       (set! i (+ i 1)))
     (string->list s))
    b))

(define bit_array_to_string bitvector->string)

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

(define (bit_array_reduce self fn #!key (initial_value 0) from_right)
  ((if from_right bitvector-fold-right bitvector-fold-left)
   fn self initial_value))

(define (bit_array_append a b)
  (let ((lena (bitvector-length a))
        (lenb (bitvector-length b)))
    (let ((self (make-bitvector (+ lena lenb))))
      (bitvector-blit! a 0 self 0 lena)
      (bitvector-blit! b 0 self lena lenb))))

(define (bit_array_concat bitvect-list)
  (if (= 1 (length bitvect-list))
      (car bitvect-list)
      (let loop ((self (car bitvect-list))
                 (bitvect-list (cdr bitvect-list)))
        (if (not (null? bitvect-list))
            (loop (bit_array_append self (car bitvect-list))
                  (cdr bitvect-list))
            self))))

(define (bit_subarray self offset count)
  (bitvector-blit! self offset (make-bitvector (- count offset)) 0 count))