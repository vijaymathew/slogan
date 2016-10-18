;; Copyright (c) 2013-2016 by Vijay Mathew Pandyalakal, All Rights Reserved.

;; Lazy-pairs.

(define-macro (lpair-cons a b) `(scm-cons ,a (delay ,b)))

(define (iterator? obj)
  (and (pair? obj)
       (s-yield? (scm-cdr obj))))

(define is_iterator iterator?)

;; Move an iterator to the next element.
(define (iter-next iter)
  (let ((r (call/cc (lambda (caller-return)
                      (let ((yield-obj (scm-cdr iter)))
                        (s-yield-k-set! yield-obj caller-return)
                        (let ((fn (s-yield-fn yield-obj)))
                          (if fn (fn) '())))))))
    (if (and (not (pair? r))
             (s-yield? r)
             (not (s-yield-fn r)))
        #f
        r)))

;; Functions for working with sequences.
;; A sequence can be either a normal list or a lazy-pair,
;; or any other object that implements the `first` and `rest`
;; generic procedures.

(define (scm-first obj)
  (if (procedure? obj)
      (obj 'first)
      (scm-head obj)))

(define (scm-rest obj)
  (if (iterator? obj)
      (iter-next obj)
      (if (procedure? obj)
          (obj 'rest)
          (scm-force (scm-tail obj)))))

(define first scm-first)

(define (rest lpair)
  (let ((r (scm-rest lpair)))
    (if (null? r) #f r)))

(define (lpair? obj)
  (and (pair? obj)
       (is_promise (scm-cdr obj))))

(define is_lpair lpair?)

(define (lpair-at i lpair)
  (if (= i 0)
      (scm-first lpair)
      (lpair-at (- i 1) (scm-rest lpair))))

(define (scm-nth n seq) 
  (if (<= n 0)
      (scm-first seq)
      (scm-nth (- n 1) (scm-rest seq))))

(define (scm-nth_tail n seq)
  (if (<= n 0) (scm-force seq)
      (scm-nth_tail (- n 1) (scm-rest seq))))
      
(define (second seq) (scm-nth 1 seq))
(define (third seq) (scm-nth 2 seq))
(define (fourth seq) (scm-nth 3 seq))
(define (fifth seq) (scm-nth 4 seq))
(define (sixth seq) (scm-nth 5 seq))
(define (seventh seq) (scm-nth 6 seq))
(define (eighth seq) (scm-nth 7 seq))
(define (ninth seq) (scm-nth 8 seq))
(define (tenth seq) (scm-nth 9 seq))

(define nth scm-nth)
(define nth_tail scm-nth_tail)

(define old-map map)

(define (iter-map fn xs more)
  (call/cc
   (lambda (*return*)
     (let ((*yield-obj* (make-s-yield #f *return*)))
       (let loop ((xs xs) (more more))
         (let ((*r* (s-yield-k *yield-obj*)))
           (cond ((or (not xs) (null? xs))
                  (s-yield-fn-set! *yield-obj* #f)
                  (*r* *yield-obj*))
                 (else
                  (call/cc
                   (lambda (*yield*)
                     (s-yield-fn-set! *yield-obj* *yield*)
                     (if (null? more)
                         (*r* (scm-cons (fn (scm-car xs)) *yield-obj*))
                         (*r* (scm-cons (scm-apply fn (scm-car xs) (old-map scm-car more)) *yield-obj*)))))
                  (loop (scm-rest xs) (if (null? more) '() (old-map rest more)))))))))))

(define (lpair-map f ls more)
  (if (null? (scm-first ls))
      '()
      (if (null? more)
          (lpair-cons (f (scm-first ls)) (lpair-map f (scm-rest ls) '()))
          (lpair-cons (scm-apply f (scm-first ls) (old-map first more))
                      (lpair-map f (scm-rest ls) (old-map rest more))))))

(define (generic-map f ls more)
  (if (null? more)
      (let map1 ((ls ls))
        (if (null? ls)
            '()
            (let ((a (f (scm-car ls)))
                  (b (map1 (scm-cdr ls))))
              (scm-cons a b))))
      (let map-more ((ls ls) (more more))
        (if (null? ls)
            '()
            (let ((a (scm-apply f (scm-car ls) (old-map car more)))
                  (b (map-more (scm-cdr ls) (old-map cdr more))))
              (scm-cons a b))))))

(define (map f ls . more)
  (cond ((or (lpair? ls) (procedure? ls))
         (lpair-map f ls more))
        ((iterator? ls)
         (iter-map f ls more))
        (else
         (generic-map f ls more))))

(define (iter-for-each f xs more)
  (let loop ((xs xs) (more more))
    (if xs
        (if (null? more)
            (begin (f (scm-first xs))
                   (loop (scm-rest xs) '()))
            (begin (apply f (scm-first xs) (old-map first more))
                   (loop (scm-rest xs) (old-map rest more)))))))

(define (generic-for-each f ls . more)
  (if (iterator? ls)
      (iter-for-each f ls more)
      (let ((lpair? (or (lpair? ls) (procedure? ls))))
        (let ((scm-car (if lpair? first car))
              (scm-cdr (if lpair? rest cdr)))
          (do ((ls ls (scm-cdr ls)) (more more (scm-map scm-cdr more)))
              ((or (not ls) (null? ls)))
            (scm-apply f (scm-car ls) (scm-map scm-car more)))))))

(define for_each generic-for-each)

;; Code for `iter-filter` was generated from this Slogan definition:
;;
;; function `iter-filter`(f, xs)
;;  let loop(xs = xs)
;;   if (not(xs))
;;    xs
;;  else
;;    if (f(first(xs)))
;;     { yield first(xs);
;;       loop(rest(xs)) }
;;    else loop(rest(xs));
;;
(define (iter-filter f xs)
  (call/cc
   (lambda (*return*)
     (let ((*yield-obj* (make-s-yield #f *return*)))
       (begin (let loop ((xs xs))
                (if (not xs)
                    xs
                    (if (f (scm-first xs))
                        (begin (call/cc
                                (lambda (*yield*)
                                  (let ((*r* (s-yield-k *yield-obj*)))
                                    (s-yield-fn-set! *yield-obj* *yield*)
                                    (*r* (scm-cons (scm-first xs) *yield-obj*)))))
                               (loop (scm-rest xs)))
                        (loop (scm-rest xs)))))
              (let ((*r* (s-yield-k *yield-obj*)))
                (s-yield-fn-set! *yield-obj* #f)
                (*r* *yield-obj*)))))))

(define (lpair-filter fn lpair #!key drill)
  (cond ((null? lpair) 
	 '())
	((and (lpair? (scm-first lpair)) drill)
	 (lpair-cons
	  (lpair-filter fn (scm-first lpair) drill: drill)
	  (lpair-filter fn (scm-rest lpair) drill: drill)))
	((fn (scm-first lpair))
	 (lpair-cons 
	  (scm-first lpair)
	  (lpair-filter fn (scm-rest lpair) drill: drill)))
	(else
	 (lpair-filter fn (scm-rest lpair) drill: drill))))      

(define (filter fn ls #!key drill)
  (cond ((lpair? ls)
         (lpair-filter fn ls drill: drill))
        ((iterator? ls)
         (iter-filter fn ls))
        (else
         (let loop ((ls ls)
                    (result '()))
           (cond ((null? ls)
                  (scm-reverse result))
                 ((and (list? (scm-car ls)) drill)
                  (loop (scm-cdr ls) (scm-cons (filter fn (scm-car ls) drill: drill) result)))
                 ((fn (scm-car ls))
                  (loop (scm-cdr ls) (scm-cons (scm-car ls) result)))
                 (else
                  (loop (scm-cdr ls) result)))))))

;; iter-accumulate generated from this Slogan definition:
;;
;; function `iter-accumulate`(f, initial, xs)
;;   let loop (initial = initial, xs = xs)
;;    if (not(xs))
;;     xs
;;    else let (r = f(initial, first(xs)))
;;         { yield r;
;;           loop(r, rest(xs)) };
;;
(define (iter-accumulate f initial xs)
  (call/cc
   (lambda (*return*)
     (let ((*yield-obj* (make-s-yield #f *return*)))
       (begin (let loop ((initial initial) (xs xs))
                (if (not xs)
                    xs
                    (let ((r (f (scm-first xs) initial)))
                      (begin (call/cc (lambda (*yield*)
                                        (let ((*r* (s-yield-k *yield-obj*)))
                                          (s-yield-fn-set! *yield-obj* *yield*)
                                          (*r* (scm-cons r *yield-obj*)))))
                             (loop r (scm-rest xs))))))
              (let ((*r* (s-yield-k *yield-obj*))) (s-yield-fn-set! *yield-obj* #f) (*r* *yield-obj*)))))))

(define (lpair-accumulate fn initial lpair)
  (if (null? lpair)
      '()
      (let ((r (fn (scm-first lpair) initial)))
        (lpair-cons r (lpair-accumulate fn r (scm-rest lpair))))))

(define (accumulate fn initial seq)
  (cond ((lpair? seq)
         (lpair-accumulate fn initial seq))
        ((iterator? seq)
         (iter-accumulate fn initial seq))
        (else
         (fold_left fn initial seq))))

(define (enumerate start end #!optional (cmpr <=) (next inc))
    (if (cmpr start end)
        (let ((elem (next start)))
          (lpair-cons start (enumerate elem end cmpr next)))
        '()))

(define (drop n lst)
  (if (or (zero? n)
	  (negative? n))
      lst
      (let loop ((lst lst)
		 (n n))
	(if (or (zero? n)
		(null? lst))
	    lst
	    (loop (scm-rest lst) (- n 1))))))

(define (take n lst)
  (if (or (zero? n)
	  (negative? n))
      '()
      (let loop ((lst lst)
		 (n n)
		 (result '()))
	(if (or (zero? n)
		(null? lst))
	    (scm-reverse result)
	    (loop (scm-rest lst)
		  (- n 1)
		  (scm-cons (scm-first lst) result))))))

(define (drop_while predic lst)
  (let loop ((lst lst))
    (if (or (null? lst)
	    (scm-not (predic (scm-first lst))))
	lst
	(loop (scm-rest lst)))))

(define (take_while predic lst)
  (let loop ((lst lst)
	     (result '()))
    (if (or (null? lst)
	    (scm-not (predic (scm-first lst))))
	(scm-reverse result)
	(loop (scm-rest lst)
	      (scm-cons (scm-first lst) result)))))

    
