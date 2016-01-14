;; Copyright (c) 2013-2016 by Vijay Mathew Pandyalakal, All Rights Reserved.

;; Lazy-pairs.

(define-macro (lpair-cons a b) `(scm-cons ,a (delay ,b)))

;; Functions for working with sequences.
;; A sequence can be either a normal list or a lazy-pair.

(define (first lpair) (head lpair))
(define (rest lpair) (force (tail lpair)))

(define (is_lpair obj)
  (and (pair? obj)
       (is_promise (scm-cdr obj))))

(define (lpair-at i lpair)
  (if (= i 0)
      (first lpair)
      (lpair-at (- i 1) (rest lpair))))

(define (nth n seq) 
  (if (<= n 0)
      (first seq)
      (nth (- n 1) (rest seq))))

(define (nth_tail n seq)
  (if (<= n 0) (force seq)
      (nth_tail (- n 1) (rest seq))))
      
(define (second seq) (nth 1 seq))
(define (third seq) (nth 2 seq))
(define (fourth seq) (nth 3 seq))
(define (fifth seq) (nth 4 seq))
(define (sixth seq) (nth 5 seq))
(define (seventh seq) (nth 6 seq))
(define (eighth seq) (nth 7 seq))
(define (ninth seq) (nth 8 seq))
(define (tenth seq) (nth 9 seq))

(define old-map map)

(define (lpair-map f ls more)
  (if (null? more)
      (lpair-cons (f (first ls)) (lpair-map f (rest ls) '()))
      (lpair-cons (apply f (first ls) (old-map first more))
                   (lpair-map f (rest ls) (old-map rest more)))))

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
            (let ((a (apply f (scm-car ls) (old-map car more)))
                  (b (map-more (scm-cdr ls) (old-map cdr more))))
              (scm-cons a b))))))

(define (map f ls . more)
  (if (is_lpair ls)
      (lpair-map f ls more)
      (generic-map f ls more)))

(define (generic-for-each f ls . more)
  (let ((lpair? (is_lpair ls)))
    (let ((scm-car (if lpair? first car))
	  (scm-cdr (if lpair? rest cdr)))
      (do ((ls ls (scm-cdr ls)) (more more (map cdr more)))
	  ((null? ls))
	(apply f (scm-car ls) (map car more))))))

(define for_each generic-for-each)

(define (lpair-filter fn lpair #!key drill)
  (cond ((null? lpair) 
	 '())
	((and (is_lpair (first lpair)) drill)
	 (lpair-cons
	  (lpair-filter fn (first lpair) drill: drill)
	  (lpair-filter fn (rest lpair) drill: drill)))
	((fn (first lpair))
	 (lpair-cons 
	  (first lpair)
	  (lpair-filter fn (rest lpair) drill: drill)))
	(else
	 (lpair-filter fn (rest lpair) drill: drill))))      

(define (filter fn ls #!key drill)
  (if (is_lpair ls)
      (lpair-filter fn ls drill: drill)
      (let loop ((ls ls)
		 (result '()))
	(cond ((null? ls)
	       (scm-reverse result))
	      ((and (list? (scm-car ls)) drill)
	       (loop (scm-cdr ls) (scm-cons (filter fn (scm-car ls) drill: drill) result)))
	      ((fn (scm-car ls))
	       (loop (scm-cdr ls) (scm-cons (scm-car ls) result)))
	      (else
	       (loop (scm-cdr ls) result))))))

(define (lpair-accumulate fn initial lpair)
  (if (null? lpair)
      '()
      (let ((r (fn initial (first lpair))))
        (lpair-cons r (lpair-accumulate fn r (rest lpair))))))

(define (accumulate fn initial seq)
  (if (is_lpair seq)
      (lpair-accumulate fn initial seq)
      (fold_right fn initial seq)))

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
	    (loop (rest lst) (- n 1))))))

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
	    (loop (rest lst)
		  (- n 1)
		  (scm-cons (first lst) result))))))

(define (drop_while predic lst)
  (let loop ((lst lst))
    (if (or (null? lst)
	    (not (predic (first lst))))
	lst
	(loop (rest lst)))))

(define (take_while predic lst)
  (let loop ((lst lst)
	     (result '()))
    (if (or (null? lst)
	    (not (predic (first lst))))
	(scm-reverse result)
	(loop (rest lst)
	      (scm-cons (first lst) result)))))

  
