;; Copyright (c) 2013-2014 by Vijay Mathew Pandyalakal, All Rights Reserved.

(define is_list list?)
(define is_pair pair?)
(define (is_atom x) (not (pair? x)))

(define (pair a b)
  (cons a b))

(define (head seq) 
  (car seq))

(define (tail seq) 
  (cdr seq))

(define (is_empty seq)
  (null? seq))

(define (at seq i)
  (list-ref seq i))

(define (get seq key)
  (let ((value (assoc key seq)))
    (if value (cdr value) #f)))

(define (put seq key value)
  (cons (cons key value) seq))

(define for_each for-each)
(define set_head set-car!)
(define set_tail set-cdr!)
(define list_to_string list->string)
(define list_to_array list->vector)
(define list_to_table list->table)

(define (filter ls fn)
  (let loop ((ls ls)
             (result '()))
    (cond ((null? ls)
           (reverse result))
          ((fn (car ls))
           (loop (cdr ls) (cons (car ls) result)))
          (else
           (loop (cdr ls) result)))))

(define (find ls test #!key (default #f))
  (let loop ((ls ls))
    (cond ((null? ls)
           default)
          ((test (car ls))
           (car ls))
          (else (loop (cdr ls))))))

(define (is_member ls e #!key (test *default-eq*))
  (let loop ((ls ls))
    (cond ((null? ls)
           #f)
          ((test e (car ls))
           #t)
          (else (loop (cdr ls))))))

(define (adjoin ls e #!key test)
  (if (not (is_member (ls e)))
      (cons e ls)
      ls))

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

(define (remove ls elem #!key (test *default-eq*) (all #f))
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

(define (reduce ls fn #!key initial_value)
  (if (null? ls)
      ls
      (begin (if (not initial_value)
		 (begin (set! initial_value (car ls))
			(set! ls (cdr ls))))
	     (let loop ((ls (cdr ls))
			(result (fn initial_value (car ls))))
	       (if (null? ls)
		   result
		   (loop (cdr ls) (fn result (car ls))))))))

;; sorting

(define (is_sorted ls #!key (test <))
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

;; 

(define (copy_list lst)
  (if (is_atom lst) 
      lst
      (cons (car lst) (copy_list (cdr lst)))))

(define (list_of n #!optional fill-with)
  (let ((opr (if (< n 0) + -)))
    (let loop ((r '())
               (n n))
      (if (zero? n)
          r
          (loop (cons fill-with r) (opr n 1))))))
