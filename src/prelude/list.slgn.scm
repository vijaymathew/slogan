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

(define (assoc_get alist key #!key 
                   (test *default-eq*)
                   (default_value #f))
  (let loop ((alist alist))
    (cond ((null? alist)
           default_value)
          ((test (caar alist) key)
           (car alist))
          (else (loop (cdr alist))))))

(define (assoc_put alist key value)
  (cons (cons key value) alist))

(define (assoc_set alist key value #!key (test *default-eq*))
  (set-cdr! (assoc_get alist key test: test) value))

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

(define (remove_duplicates ls #!key (test *default-eq*))
  (let loop ((ls ls)
             (result '()))
    (cond ((null? ls) 
           (reverse result))
          (else 
           (if (is_member result (car ls))
               (loop (cdr ls) result)
               (loop (cdr ls) (cons (car ls) result)))))))

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

(define (quicksort l #!optional (test <))
  (if (null? l)
      '()
      (append (quicksort (filter (cdr l) (lambda (x) (not (test (car l) x)))) test)
              (list (car l))
              (quicksort (filter (cdr l) (lambda (x) (test (car l) x))) test))))

(define (+merge+ xs ys test) 
  (cond ((and (is_empty xs) 
              (not (is_empty ys)))
         ys)
        ((and (is_empty ys) 
              (not (is_empty xs)))
         xs) 
        ((test (head xs) (head ys))
         (cons (head xs) 
               (+merge+ (tail xs) ys test)))
        (else 
         (cons (head ys) (+merge+ xs (tail ys) test)))))

(define (+split+ xs)
  (letrec ((split_helper (lambda (xs ys zs) 
                           (cond ((is_empty xs) 
                                  (cons ys zs))
                                 ((eqv? (length xs) 1) 
                                  (cons (cons (head xs) ys) zs))
                                 (else (split_helper (tail (tail xs)) 
                                                     (cons (head xs) ys) 
                                                     (cons (head (tail xs)) zs)))))))
    (split_helper xs '() '())))

(define (mergesort xs #!optional (test <))
  (if (or (is_empty xs) (eqv? (length xs) 1)) 
      xs 
      (let ((parts (+split+ xs))) 
        (+merge+ (mergesort (head parts) test) (mergesort (tail parts) test) test))))

(define (sort ls #!key (test <) (type 'quick))
  (case type
    ((quick) (quicksort ls test))
    ((merge) (mergesort ls test))
    (else (error "sorting algorithm not implemented. " type))))

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

(define (replace lst a b #!key (test eqv?))
  (let loop ((lst lst)
             (result '()))
    (cond ((null? lst)
           (reverse result))
          ((list? (car lst))
           (loop (cdr lst) (cons (replace (car lst) a b test: test) result)))
          ((test a (car lst))
           (loop (cdr lst) (cons b result)))
          (else
           (loop (cdr lst) (cons (car lst) result))))))

(define (replace_all lst alst blst #!key (test eqv?))
  (let loop ((lst lst)
             (result '()))
    (cond ((null? lst)
           (reverse result))
          ((list? (car lst))
           (loop (cdr lst) (cons (replace_all (car lst) alst blst test: test) result)))
          (else
           (let inner-loop ((alst alst)
                            (blst blst))
             (cond ((not (null? alst))
                    (if (test (car alst) (car lst))
                        (loop (cdr lst) (cons (car blst) result))
                        (inner-loop (cdr alst) (cdr blst))))
                   (else 
                    (loop (cdr lst) (cons (car lst) result)))))))))