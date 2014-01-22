;; Copyright (c) 2013-2014 by Vijay Mathew Pandyalakal, All Rights Reserved.

(define (pair a b)
  (cons a b))

(define (first seq) 
  (car seq))

(define (rest seq) 
  (cdr seq))

(define (empty? seq)
  (null? seq))

(define (at seq i)
  (list-ref seq i))

(define (get seq key)
  (let ((value (assoc key seq)))
    (if value (cdr value) #f)))

(define (put seq key value)
  (cons (cons key value) seq))

(define set_first set-car!)
(define set_rest set-cdr!)
(define tail list-tail)
(define list_to_string list->string)
(define list_to_array list->vector)

(define (contains? ls fn)
  (let loop ((ls ls))
    (cond ((null? ls)
           #f)
          ((fn (car ls))
           #t)
          (else (loop (cdr ls))))))

(define (filter ls fn)
  (let loop ((ls ls)
             (result '()))
    (cond ((null? ls)
           (reverse result))
          ((fn (car ls))
           (loop (cdr ls) (cons (car ls) result)))
          (else
           (loop (cdr ls) result)))))

(define (find ls fn)
  (let loop ((ls ls))
    (cond ((null? ls)
           #f)
          ((fn (car ls))
           (car ls))
          (else (loop (cdr ls))))))

(define (smallest ls #!key (test <))
  (if (null? ls) 
      #f
      (let loop ((ls ls) 
                 (s (car ls)))
        (cond ((null? ls)
               s)
              ((test (car ls) s)
               (loop (cdr ls) (car ls)))
              (else (loop (cdr ls) s))))))

(define (remove ls elem #!key (test equal?) (all #f))
  (let loop ((ls ls)
             (removed #f)
             (result '()))
    (cond ((null? ls)
           (reverse result))
          ((and (not removed)
                (test (car ls) elem))
           (loop (cdr ls) (if all #f #t) result))
          (else 
           (loop (cdr ls) removed (cons (car ls) result))))))

;; sorting

(define (sorted? ls #!key (test <))
  (let loop ((ls ls))
    (cond ((or (null? ls) (= (length ls) 1))
           #t)
          ((test (cadr ls) (car ls))
           #f)
          (else (loop (cdr ls))))))

(define (sort ls #!key (test <) (type '!quick))
  (let ((arr (list->vector ls)))
    (array_sort arr test: test type: type)
    (vector->list arr)))
    
