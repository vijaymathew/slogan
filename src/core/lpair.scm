;; Copyright (c) 2013-2017 by Vijay Mathew Pandyalakal, All Rights Reserved.

;; Lazy-pairs.

(define-macro (lpair-cons a b) `(scm-cons ,a (delay ,b)))

(define (empty? obj) (or (null? obj) (eq? obj #f)))

(define is_empty empty?)

(define (iterator? obj)
  (and (pair? obj)
       (s-yield? (scm-cdr obj))))

(define is_iterator iterator?)

;; Move an iterator to the next element.
(define (iter-next iter #!optional (arg *void*))
  (let ((r (call/cc (lambda (caller-return)
                      (let ((yield-obj (scm-cdr iter)))
                        (s-yield-k-set! yield-obj caller-return)
                        (let ((fn (s-yield-fn yield-obj)))
                          (if fn (fn arg) '())))))))
    (if (and (scm-not (pair? r))
             (s-yield? r)
             (scm-not (s-yield-fn r)))
        #f
        r)))

(define next iter-next)

;; Functions for working with sequences.
;; A sequence can be either a normal list, lazy-pair, an array
;; or any other object that implements the `first` and `rest`
;; generic procedures.

(define (scm-first obj)
  (cond
   ((pair? obj)
    (scm-head obj))
   ((procedure? obj)
    (obj 'first))
   ((null? obj)
    #f)
   (else
    (safe-generic-array-first obj))))

(define (rest-helper obj)
  (cond
   ((pair? obj)
    (let ((t (scm-tail obj)))
      (if t
          (cond
           ((s-yield? t)
            (iter-next obj))
           ((##promise? t)
            (scm-force t))
           (else t))
          t)))
   ((procedure? obj)
    (obj 'rest))
   (else (safe-generic-array-rest obj))))

(define first scm-first)

(define (scm-rest lpair)
  (let ((r (rest-helper lpair)))
    (if (null? r) #f r)))

(define rest scm-rest)

(define (lpair? obj)
  (and (pair? obj)
       (##promise? (scm-cdr obj))))

(define is_lpair lpair?)

(define (lpair-at i lpair)
  (if (scm-= i 0)
      (scm-first lpair)
      (lpair-at (scm-- i 1) (rest-helper lpair))))

(define (scm-nth n seq) 
  (if (scm-<= n 0)
      (scm-first seq)
      (scm-nth (scm-- n 1) (rest-helper seq))))

(define (scm-nth_tail n seq)
  (if (scm-<= n 0) (scm-force seq)
      (scm-nth_tail (scm-- n 1) (rest-helper seq))))
      
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
           (cond ((or (scm-not xs) (null? xs))
                  (s-yield-fn-set! *yield-obj* #f)
                  (*r* *yield-obj*))
                 (else
                  (call/cc
                   (lambda (*yield*)
                     (s-yield-fn-set! *yield-obj* *yield*)
                     (if (null? more)
                         (*r* (scm-cons (fn (scm-car xs)) *yield-obj*))
                         (*r* (scm-cons (scm-apply fn (scm-car xs) (old-map scm-car more)) *yield-obj*)))))
                  (loop (rest-helper xs) (if (null? more) '() (old-map rest-helper more)))))))))))

(define (lpair-map f ls more)
  (if (null? (scm-first ls))
      '()
      (if (null? more)
          (lpair-cons (f (scm-first ls)) (lpair-map f (rest-helper ls) '()))
          (lpair-cons (scm-apply f (scm-first ls) (old-map scm-first more))
                      (lpair-map f (rest-helper ls) (old-map rest-helper more))))))

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
            (let ((a (scm-apply f (scm-car ls) (old-map scm-car more)))
                  (b (map-more (scm-cdr ls) (old-map scm-cdr more))))
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
                   (loop (rest-helper xs) '()))
            (begin (scm-apply f (scm-first xs) (old-map scm-first more))
                   (loop (rest-helper xs) (old-map rest-helper more)))))))

(define (generic-for-each f ls . more)
  (if (iterator? ls)
      (iter-for-each f ls more)
      (let ((lpair? (or (lpair? ls) (procedure? ls))))
        (let ((scm-car (if lpair? scm-first scm-car))
              (scm-cdr (if lpair? rest-helper scm-cdr)))
          (do ((ls ls (scm-cdr ls)) (more more (scm-map scm-cdr more)))
              ((or (scm-not ls) (null? ls)))
            (scm-apply f (scm-car ls) (scm-map scm-car more)))))))

(define for_each generic-for-each)

;; Code for `iter-filter` was generated from this Slogan definition:
;;
;; function `iter-filter`(f, xs)
;;  let loop(xs = xs)
;;   if (not (xs))
;;    xs
;;  else
;;    if (f(first(xs)))
;;     { yield first(xs)
;;       loop(rest(xs)) }
;;    else loop(rest(xs))
;;
(define (iter-filter f xs)
  (call/cc
   (lambda (*return*)
     (let ((*yield-obj* (make-s-yield #f *return*)))
       (begin (let loop ((xs xs))
                (if (scm-not xs)
                    xs
                    (if (f (scm-first xs))
                        (begin (call/cc
                                (lambda (*yield*)
                                  (let ((*r* (s-yield-k *yield-obj*)))
                                    (s-yield-fn-set! *yield-obj* *yield*)
                                    (*r* (scm-cons (scm-first xs) *yield-obj*)))))
                               (loop (rest-helper xs)))
                        (loop (rest-helper xs)))))
              (let ((*r* (s-yield-k *yield-obj*)))
                (s-yield-fn-set! *yield-obj* #f)
                (*r* *yield-obj*)))))))

(define (lpair-filter fn lpair #!key drill)
  (cond ((null? lpair) 
	 '())
	((and (lpair? (scm-first lpair)) drill)
	 (lpair-cons
	  (lpair-filter fn (scm-first lpair) drill: drill)
	  (lpair-filter fn (rest-helper lpair) drill: drill)))
	((fn (scm-first lpair))
	 (lpair-cons 
	  (scm-first lpair)
	  (lpair-filter fn (rest-helper lpair) drill: drill)))
	(else
	 (lpair-filter fn (rest-helper lpair) drill: drill))))

(define (scm-filter fn ls #!key drill)
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
                  (loop (scm-cdr ls) (scm-cons (scm-filter fn (scm-car ls) drill: drill) result)))
                 ((fn (scm-car ls))
                  (loop (scm-cdr ls) (scm-cons (scm-car ls) result)))
                 (else
                  (loop (scm-cdr ls) result)))))))

(define filter scm-filter)

;; iter-accumulate generated from this Slogan definition:
;;
;; function `iter-accumulate`(f, initial, xs)
;;   let loop (initial = initial, xs = xs)
;;    if (not (xs))
;;     xs
;;    else let (r = f(initial, first(xs)))
;;    { yield r
;;      loop(r, rest(xs)) }
;;
(define (iter-accumulate f initial xs)
  (call/cc
   (lambda (*return*)
     (let ((*yield-obj* (make-s-yield #f *return*)))
       (begin (let loop ((initial initial) (xs xs))
                (if (scm-not xs)
                    xs
                    (let ((r (f (scm-first xs) initial)))
                      (begin (call/cc (lambda (*yield*)
                                        (let ((*r* (s-yield-k *yield-obj*)))
                                          (s-yield-fn-set! *yield-obj* *yield*)
                                          (*r* (scm-cons r *yield-obj*)))))
                             (loop r (rest-helper xs))))))
              (let ((*r* (s-yield-k *yield-obj*))) (s-yield-fn-set! *yield-obj* #f) (*r* *yield-obj*)))))))

(define (lpair-accumulate fn initial lpair)
  (if (null? lpair)
      '()
      (let ((r (fn (scm-first lpair) initial)))
        (lpair-cons r (lpair-accumulate fn r (rest-helper lpair))))))

(define (accumulate fn initial seq)
  (cond ((lpair? seq)
         (lpair-accumulate fn initial seq))
        ((iterator? seq)
         (iter-accumulate fn initial seq))
        (else
         (scm-fold_left fn initial seq))))

(define (enumerate start end #!key (compare <=) (next scm-inc))
    (if (compare start end)
        (let ((elem (next start)))
          (lpair-cons start (enumerate elem end compare: compare next: next)))
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
	    (loop (rest-helper lst) (scm-- n 1))))))

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
	    (loop (rest-helper lst)
		  (scm-- n 1)
		  (scm-cons (scm-first lst) result))))))

(define (drop_while predic lst)
  (let loop ((lst lst))
    (if (or (null? lst)
	    (scm-not (predic (scm-first lst))))
	lst
	(loop (rest-helper lst)))))

(define (take_while predic lst)
  (let loop ((lst lst)
	     (result '()))
    (if (or (null? lst)
	    (scm-not (predic (scm-first lst))))
	(scm-reverse result)
	(loop (rest-helper lst)
	      (scm-cons (scm-first lst) result)))))

(define (realize n xs)
  (let loop ((ys '())
             (n n)
             (rs xs))
    (if (scm-<= n 0)
        (scm-cons (scm-reverse ys) rs)
        (let ((f (scm-first rs)))
          (if f
              (loop (scm-cons f ys) (scm-- n 1) (scm-rest rs))
              (loop ys -1 (scm-rest rs)))))))

(define (find predic ls #!key default)
  (let loop ((ls ls))
    (cond ((scm-not ls) default)
          ((predic (scm-first ls)) (scm-first ls))
          (else (loop (scm-rest ls))))))
