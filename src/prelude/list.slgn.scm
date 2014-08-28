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

(define for_each for-each)
(define set_head set-car!)
(define set_tail set-cdr!)
(define list_to_string list->string)
(define list_to_array list->vector)

(define (list_to_table lst #!key size init 
                       weak_keys weak_values 
                       (test equal?) hash 
                       (min_load 0.45)
                       (max_load 0.90))
  (cond ((and size init hash)
         (list->table lst size: size init: init weak-keys: weak_keys
                      weak-values: weak_values test: test
                      hash: hash min-load: min_load max-load: max_load))
        ((and size init)
         (list->table lst size: size init: init weak-keys: weak_keys
                      weak-values: weak_values test: test
                      min-load: min_load max-load: max_load))
        (size
         (list->table lst size: size weak-keys: weak_keys
                      weak-values: weak_values test: test
                      min-load: min_load max-load: max_load))
        (else 
         (list->table lst weak-keys: weak_keys
                      weak-values: weak_values test: test
                      min-load: min_load max-load: max_load))))

(define (filter fn ls #!key drill)
  (let loop ((ls ls)
             (result '()))
    (cond ((null? ls)
           (reverse result))
          ((and (list? (car ls)) drill)
           (loop (cdr ls) (cons (filter fn (car ls) drill: drill) result)))
          ((fn (car ls))
           (loop (cdr ls) (cons (car ls) result)))
          (else
           (loop (cdr ls) result)))))

(define (memp predic ls #!key default)
  (cond ((null? ls) default)
        ((predic (car ls)) ls)
        (else (memp predic (cdr ls) default: default))))

(define (rem-helper obj ls predic)
  (let loop ((ls ls)
             (r '()))
    (cond ((null? ls) (reverse r))
          ((predic obj (car ls)) (loop (cdr ls) r))
          (else (loop (cdr ls) (cons (car ls) r))))))

(define (remq obj ls) (rem-helper obj ls eq?))
(define (remv obj ls) (rem-helper obj ls eqv?))
(define (remove obj ls) (rem-helper obj ls equal?))

(define (remp predic ls) 
  (let loop ((ls ls)
             (r '()))
    (cond ((null? ls) (reverse r))
          ((predic (car ls)) (loop (cdr ls) r))
          (else (loop (cdr ls) (cons (car ls) r))))))

;; sorting

(define (quicksort l #!optional (test <))
  (if (null? l) '()
      (append (quicksort (filter 
                          (lambda (x) (not (test (car l) x)))
                          (cdr l)) 
                         test)
              (list (car l))
              (quicksort (filter 
                          (lambda (x) (test (car l) x))
                          (cdr l))
                         test))))

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
  (letrec ((split_helper 
            (lambda (xs ys zs) 
              (cond ((is_empty xs) 
                     (cons ys zs))
                    ((eqv? (length xs) 1) 
                     (cons (cons (head xs) ys) zs))
                    (else (split_helper 
                           (tail (tail xs)) 
                           (cons (head xs) ys) 
                           (cons (head (tail xs)) zs)))))))
    (split_helper xs '() '())))

(define (mergesort xs #!optional (test <))
  (if (or (is_empty xs) (eqv? (length xs) 1)) xs 
      (let ((parts (+split+ xs))) 
        (+merge+ (mergesort (head parts) test) 
                 (mergesort (tail parts) test) test))))

(define (sort ls #!optional (test <) (type 'quick))
  (case type
    ((quick) (quicksort ls test))
    ((merge) (mergesort ls test))
    (else (error "sorting algorithm not implemented. " type))))

;; 

(define (copy_list lst)
  (if (not (pair? lst))
      (error "(Argument 1) PAIR expected\n" lst))
  (if (not (list? lst))
      (cons (car lst) (cdr lst))
      (let loop ((lst lst) (result '()))
        (cond ((null? lst) (reverse result))
              ((is_atom lst) (reverse (cons lst result)))
              (else (loop (cdr lst) (cons (car lst) result)))))))

(define (list_of n #!optional fill-with)
  (let ((opr (if (< n 0) + -)))
    (let loop ((r '())
               (n n))
      (if (zero? n) r
          (loop (cons fill-with r) (opr n 1))))))

(define (nth_tail lst n)
  (if (= n 0) lst
      (list-tail (cdr lst) (- n 1))))

(define (drop lst n)
  (let ((neg (< n 0)))
    (let loop ((lst (if neg (reverse lst) lst))
               (n (if neg (- n) n))
               (result '()))
      (cond ((null? lst)
             (if neg result (reverse result)))
            ((zero? n)
             (loop (cdr lst) 0 (cons (car lst) result)))
            (else (loop (cdr lst) (- n 1) result))))))

(define (take lst n)
  (let ((neg (< n 0)))
    (let loop ((lst (if neg (reverse lst) lst))
               (n (if neg (- n) n))
               (result '()))
      (if (or (null? lst)
              (= n 0))
          (if neg result (reverse result))
          (loop (cdr lst) 
                (- n 1) 
                (cons (car lst) result))))))

(define (partition predic ls)
  (let loop ((ls ls) (first '()) (second '()))
    (cond ((null? ls) (cons (reverse first) (reverse second)))
          ((predic (car ls)) (loop (cdr ls) (cons (car ls) first) second))
          (else (loop (cdr ls) first (cons (car ls) second))))))

(define (find predic ls #!key default)
  (let loop ((ls ls))
    (cond ((null? ls) default)
          ((predic (car ls)) (car ls))
          (else (loop (cdr ls))))))

(define (assp predic ls #!key default)
  (let loop ((ls ls))
    (cond ((null? ls) default)
          ((predic (caar ls)) (car ls))
          (else (loop (cdr ls))))))

(define (zip a b)
  (let loop ((a a) (b b) (result '()))
    (if (or (null? a) (null? b))
        (reverse result)
        (loop (cdr a) (cdr b) (cons (cons (car a) (car b)) result)))))

(define (exists f ls . more)
  (if (not (null? more)) 
      (assert-equal-lengths ls more))
  (and (not (null? ls))
       (let exists ((x (car ls)) (ls (cdr ls)) (more more))
         (if (null? ls)
             (apply f x (map car more))
             (or (apply f x (map car more))
                 (exists (car ls) (cdr ls) (map cdr more)))))))

(define (for_all f ls . more)
  (if (not (null? more)) 
      (assert-equal-lengths ls more))
  (or (null? ls)
      (let for-all ((x (car ls)) (ls (cdr ls)) (more more))
        (if (null? ls)
            (apply f x (map car more))
            (and (apply f x (map car more))
                 (for-all (car ls) (cdr ls) (map cdr more)))))))

(define (fold_left f obj ls . more)
  (if (not (null? more))
      (assert-equal-lengths ls more))
  (let fold-left ((obj obj) (ls ls) (more more))
    (if (null? ls) obj
        (fold-left (apply f obj (car ls) (map car more)) 
                   (cdr ls) 
                   (map cdr more)))))


;; Taken from http://srfi.schemers.org/srfi-1/srfi-1-reference.scm:
(define (%cdrs lists)
  (call-with-current-continuation
    (lambda (abort)
      (let recur ((lists lists))
	(if (pair? lists)
	    (let ((lis (car lists)))
	      (if (null? lis) (abort '())
		  (cons (cdr lis) (recur (cdr lists)))))
	    '())))))

(define (%cars+ lists last-elt)	; (append! (map car lists) (list last-elt))
  (let recur ((lists lists))
    (if (pair? lists) (cons (caar lists) (recur (cdr lists))) (list last-elt))))

(define (fold_right f obj ls . more)
  (if (not (null? more))
      (assert-equal-lengths ls more))
  (if (pair? more)
      (let recur ((lists (cons ls more)))
	(let ((cdrs (%cdrs lists)))
	  (if (null? cdrs) obj
	      (apply f (%cars+ lists (recur cdrs))))))
      (let recur ((ls ls))
	(if (null? ls) obj
	    (let ((head (car ls)))
	      (f head (recur (cdr ls))))))))
;; :~
