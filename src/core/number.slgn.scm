;; Copyright (c) 2013-2017 by Vijay Mathew Pandyalakal, All Rights Reserved.

(define is_number number?)
(define is_integer integer?)
(define is_real real?)
(define is_rational rational?)
(define is_complex complex?)
(define is_exact exact?)
(define is_inexact inexact?)
(define is_zero zero?)
(define is_even even?)
(define is_odd odd?)
(define is_positive positive?)
(define is_negative negative?)
(define is_nan nan?)

(define inf +inf.0)
(define _inf -inf.0)
(define nan +nan.0)
(define _zero -0.)
(define zero 0.)

(define is_finite finite?)
(define is_infinite infinite?)
(define (is_positive_infinity n) (= n +inf.0))
(define (is_negative_infinity n) (= n -inf.0))

(define (exact num)
  (if (exact? num) num
      (inexact->exact num)))

(define (inexact num)
  (if (inexact? num) num
      (exact->inexact num)))

(define integer_to_char integer->char)
(define number_to_string number->string)
(define fixnum_to_flonum fixnum->flonum)

(define rectangular make-rectangular)
(define polar make-polar)
(define real_part real-part)
(define imag_part imag-part)

(define add +)
(define sub -)
(define mult *)
(define div /)

;; for real numbers
(define (quo x1 x2) (scm-truncate (/ x1 x2)))
(define scm-quo quo)
(define (rem x1 x2) (- x1 (* x2 (scm-quo x1 x2))))
(define (mod x1 x2) (- x1 (* x2 (scm-floor (/ x1 x2)))))

(define scm-mod mod)

(define (sub1 n) (- n 1))
(define (add1 n) (+ n 1))

(define (safe_div a b)
  (if (zero? b) +inf.0 (/ a b)))

(define (logb x b) (/ (scm-log x) (scm-log b)))

(define is_fixnum fixnum?)
(define (least_fixnum) ##min-fixnum)
(define (greatest_fixnum) ##max-fixnum)
(define fx_is_negative fxnegative?)
(define fx_is_positive fxpositive?)
(define fx_is_zero fxzero?)
(define fx_is_even fxeven?)
(define fx_is_odd fxodd?)

(define fxadd fx+)
(define fxsub fx-)
(define fxmult fx*)

(define (fxdiv f1 f2)
  (if (scm-not (fixnum? f1))
      (scm-error "(Argument 1) FIXNUM expected"))
  (if (scm-not (fixnum? f2))
      (scm-error "(Argument 2) FIXNUM expected"))
  (inexact->exact (scm-floor (/ f1 f2))))

(define fxmod fxmodulo)
(define fxadd_wrap fxwrap+)
(define fxsub_wrap fxwrap-)
(define fxmult_wrap fxwrap*)

(define *fixnum-width* (- (inexact->exact (+ 2 (scm-floor (logb ##max-fixnum 2)))) 1))
(define (fixnum_width) *fixnum-width*)

(define fx_is_eq fx=)
(define fx_is_lt fx<)
(define fx_is_gt fx>)
(define fx_is_lteq fx<=)
(define fx_is_gteq fx>=)

(define (fxbit_is_set fx idx)
  (fxbit-set? idx fx))

(define fxshift fxarithmetic-shift)
(define fxshift_left fxarithmetic-shift-left)
(define fxshift_right fxarithmetic-shift-right)

(define (fxbit_count fx)
  (if (fx>= fx 0) (fxbit-count fx)
      (fxnot (fxbit-count (fxnot fx)))))

(define fxfirst_bit_set fxfirst-bit-set)
(define fxshift_wrap fxwraparithmetic-shift)
(define fxshift_left_wrap fxwraparithmetic-shift-left)
(define fxshift_right_wrap fxwraplogical-shift-right)
(define fxlshift_right_wrap fxwraplogical-shift-right)

(define fladd fl+)
(define flsub fl-)
(define flmult fl*)
(define fldiv fl/)

(define (flmod fl1 fl2)
  (if (scm-not (flonum? fl1))
      (scm-error "(Argument 1) FLONUM expected"))
  (if (scm-not (flonum? fl2))
      (scm-error "(Argument 2) FLONUM expected"))
  (scm-mod fl1 fl2))

(define is_flonum flonum?)
(define fl_is_finite flfinite?)
(define fl_is_infinite flinfinite?)
(define fl_is_even fleven?)
(define fl_is_odd flodd?)
(define fl_is_integer flinteger?)
(define fl_is_nan flnan?)
(define fl_is_zero flzero?)
(define fl_is_negative flnegative?)
(define fl_is_positive flpositive?)

(define fl_is_eq fl=)
(define fl_is_lt fl<)
(define fl_is_gt fl>)
(define fl_is_lteq fl<=)
(define fl_is_gteq fl>=)

(define scm-fllog fllog)

(define (fllogb x b) (fl/ (scm-fllog x) (scm-fllog b)))

(define bshift arithmetic-shift)
(define bmerge bitwise-merge)
(define band bitwise-and)
(define bior bitwise-ior)
(define bxor bitwise-xor)
(define bnot bitwise-not)

(define (bif i1 i2 i3)
  (bitwise-ior 
   (bitwise-and i1 i2)
   (bitwise-and (bitwise-not i1) i3)))

(define (bit_count i)
  (if (>= i 0) (bit-count i)
      (bitwise-not (bit_count (bitwise-not i)))))

(define (is_bit_set i n) (bit-set? n i))

(define blength integer-length)

(define (copy_bit i index bit)
  (if (scm-not (or (= bit 1) (= bit 0)))
      (scm-error "Bit flag must be either 0 or 1."))
  (if (= bit 1)
      (bitwise-ior i (arithmetic-shift 1 index))
      (bitwise-and i (bitwise-not (arithmetic-shift 1 index)))))

(define (assert-nonneg-int i msg)
  (if (or (scm-not (integer? i)) (< i 0))
      (scm-error msg i)))

(define (assert-bw-range start end)
  (assert-nonneg-int start "Start index must be a nonnegative integer.")
  (assert-nonneg-int end "End index must be a nonnegative integer.")
  (if (> start end)
      (scm-error "End index is less that start index." start end)))
  
(define (bit_field i start end)
  (assert-bw-range start end)
  (extract-bit-field (- end start) start i))

(define (copy_bit_field to start end from)
  (assert-bw-range start end)
  (let* ((mask1 (arithmetic-shift -1 start))
         (mask2 (bitwise-not (arithmetic-shift -1 end)))
         (mask (bitwise-and mask1 mask2)))
    (bif mask (arithmetic-shift from start) to)))

(define (rotate_bit_field n start end count)
  (assert-bw-range start end)
  (assert-nonneg-int count "Count field must be a nonnegative integer.")
  (let* ((width (- end start))
         (count (scm-modulo count width))
         (field0 (bit_field n start end))
         (field1 (arithmetic-shift field0 count))
         (field2 (arithmetic-shift field0 (- count width)))
         (field (bitwise-ior field1 field2)))
    (copy_bit_field n start end field)))

(define (reverse_bit_field n start end)
  (assert-bw-range start end)
  (let ((field (bit_field n start end))
        (width (- end start)))
    (let loop ((old field)(new 0)(width width))
      (cond
       ((zero? width) (copy_bit_field n start end new))
       (else (loop (arithmetic-shift old -1)
                   (bitwise-ior (arithmetic-shift new 1)
                                (bitwise-and old 1))
                   (sub1 width)))))))

(define is_any_bits_set any-bits-set?)
(define is_all_bits_set all-bits-set?)
(define first_bit_set first-bit-set)

(define number_is_eq =)
(define number_is_lt <)
(define number_is_gt >)
(define number_is_lteq <=)
(define number_is_gteq >=)

(define integer_sqrt integer-sqrt)
(define integer_nth_root integer-nth-root)

(define random_integer random-integer)
(define random_real random-real)
(define random_byte_array random-u8vector)

(define (default_random_source) default-random-source)
(define (set_default_random_source s) (set! default-random-source s))
(define random_source make-random-source)
(define is_random_source random-source?)
(define random_source_state random-source-state-ref)
(define random_source_set_state random-source-state-set!)
(define random_source_randomize random-source-randomize!)
(define random_source_pseudo_randomize random-source-pseudo-randomize!)
(define random_source_for_integers random-source-make-integers)
(define random_source_for_reals random-source-make-reals)
(define random_source_for_byte_arrays random-source-make-u8vectors)

(define (prime? n)
  (if (< n 4) (> n 1)
      (and (odd? n)
	   (let loop ((k 3))
	     (or (> (* k k) n)
		 (and (positive? (scm-remainder n k))
		      (loop (+ k 2))))))))

(define is_prime prime?)

(define (inc n) (+ n 1))
(define (dec n) (- n 1))
