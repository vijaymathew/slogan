;; Copyright (c) 2013-2014 by Vijay Mathew Pandyalakal, All Rights Reserved.

(define array make-vector)
(define array_length vector-length)
(define array_at vector-ref)
(define array_set vector-set!)
(define array_to_list vector->list)
(define list_to_array list->vector)

(define (array_swap arr i j)
  (let ((tmp (vector-ref arr i)))
    (vector-set! arr i (vector-ref arr j))
    (vector-set! arr j tmp)))

(define (array_smallest arr #!key (test <) (start 0) (end -1))
  (let loop ((start (+ start 1))
             (end (if (= -1 end) (vector-length arr) end))
             (min start))
    (cond ((>= start end)
           min)
          ((test (vector-ref arr start) (vector-ref arr min))
           (loop (+ start 1) end start))
          (else (loop (+ start 1) end min)))))

;; sorting

(define (array_sorted? arr #!key (test <))
  (let ((len (vector-length arr)))
    (if (<= len 1)
        #t
        (let loop ((i 0))
          (cond ((< i len)
                 (if (test (vector-ref arr (+ i 1))
                           (vector-ref arr i))
                     #f
                     (loop (+ i 1))))
                (else #t))))))

(define (selection-sort arr test)
  (let ((len (vector-length arr)))
    (let loop ((i 0))
      (cond ((< i len)
             (array_swap 
              arr i (array_smallest arr 
                                    test: test
                                    start: i
                                    end: len))
             (loop (+ i 1)))))))

(define (insertion-sort arr test)
  (let ((len (vector-length arr)))
    (let loop ((i 1))
      (cond ((< i len)
             (let inner-loop ((j i))
               (if (and (> j 0)
                        (test (vector-ref arr j)
                              (vector-ref arr (- j 1))))
                   (begin (array_swap arr j (- j 1))
                          (inner-loop (- j 1)))))
             (loop (+ i 1)))))))

(define (quick-sort arr test)
  (define (partition items lo hi)
    (let ((j (let loop ((i (+ lo 1)) (j hi)
                        (v (vector-ref items lo)))
               (let inner-loop ((p (test (vector-ref items i) v)))
                 (if p
                     (begin (set! i (+ i 1))
                            (if (not (= i hi))
                                (inner-loop (test (vector-ref items i) v))))))
               (let inner-loop ((p (test v (vector-ref items j))))
                 (if p
                     (begin (set! j (- j 1))
                            (if (not (= j lo))
                                (inner-loop (test v (vector-ref items j)))))))
               (if (not (>= i j))
                   (begin (array_swap items i j)
                          (loop i j v))
                   j))))
      (array_swap items lo j)
      j))

  (define (sort-helper items lo hi)
    (if (not (<= hi lo))
        (let ((j (partition items lo hi)))
          (sort-helper items lo (- j 1))
          (sort-helper items (+ j 1) hi))))

  (sort-helper arr 0 (- (vector-length arr) 1)))

(define (invalid-sort arr test)
  (error "not a valid sort type."))

(define (array_sort arr #!key (test <) (type '!quick))
  ((case type
     ((!insertion) insertion-sort)
     ((!quick) quick-sort)
     ((!selection) selection-sort)
     (else invalid-sort))
   arr test))