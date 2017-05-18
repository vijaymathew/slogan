;; Copyright (c) 2013-2017 by Vijay Mathew Pandyalakal, All Rights Reserved.

(define is_list list?)
(define is_pair pair?)

(define (atom? x)
  (and (scm-not (pair? x))
       (scm-not (null? x))))

(define is_atom atom?)

(define (pair a b)
  (scm-cons a b))

(define (scm-head seq)
  (if (pair? seq)
      (scm-car seq)
      #f))

(define (scm-tail seq) 
  (if (pair? seq)
      (scm-cdr seq)
      #f))

(define head scm-head)
(define tail scm-tail)

(define is_empty null?)

(define at list-ref)

(define (get seq key #!optional deflaut)
  (let ((mapping (scm-assoc key seq)))
    (if mapping (scm-cdr mapping) deflaut)))        

(define (list-set! list k val)
    (if (zero? k)
        (set-car! list val)
        (list-set! (scm-cdr list) (scm-- k 1) val)))

(define (set-at! n v xs)
  (if (or (null? xs) (scm-< n 0))
      *void*
      (let loop ((n n) (ys xs) (rs '()))
        (cond
         ((null? ys) xs)
         ((scm-= n 0)
          (scm-append (scm-reverse (scm-cons v rs)) (if (null? ys) ys (scm-cdr ys))))
         (else
          (loop (scm-- n 1) (scm-cdr ys) (scm-cons (scm-car ys) rs)))))))

(define list_set list-set!)
(define set_head set-car!)
(define set_tail set-cdr!)
(define list_to_string list->string)
(define list_to_array list->vector)

(define (scm-memp predic ls #!key default)
  (cond ((null? ls) default)
        ((predic (scm-car ls)) ls)
        (else (scm-memp predic (scm-cdr ls) default: default))))

(define memp scm-memp)

(define (rem-helper obj ls predic)
  (let loop ((ls ls)
             (r '()))
    (cond ((null? ls) (scm-reverse r))
          ((predic obj (scm-car ls)) (loop (scm-cdr ls) r))
          (else (loop (scm-cdr ls) (scm-cons (scm-car ls) r))))))

(define (remq obj ls) (rem-helper obj ls eq?))
(define (remv obj ls) (rem-helper obj ls eqv?))
(define (remove obj ls) (rem-helper obj ls equal?))

(define (remp predic ls) 
  (let loop ((ls ls)
             (r '()))
    (cond ((null? ls) (scm-reverse r))
          ((predic (scm-car ls)) (loop (scm-cdr ls) r))
          (else (loop (scm-cdr ls) (scm-cons (scm-car ls) r))))))

;; sorting

(define (qs-partition xs m test)
  (let loop ((xs xs) (as '()) (bs '()))
    (if (null? xs)
        (scm-cons as bs)
        (let ((x (scm-car xs)))
          (if (test x m)
              (loop (scm-cdr xs) (scm-cons x as) bs)
              (loop (scm-cdr xs) as (scm-cons x bs)))))))

;; TODO: a more efficient, tail-recursive implementation.
(define (quicksort l #!optional (test lt-compare))
  (if (null? l) '()
      (let* ((x (scm-car l))
             (parts (qs-partition (scm-cdr l) x test)))
        (scm-append (quicksort (scm-car parts) test)
                    (scm-list x)
                    (quicksort (scm-cdr parts) test)))))

(define (+merge+ xs ys test) 
  (cond ((and (null? xs) 
              (scm-not (null? ys)))
         ys)
        ((and (null? ys) 
              (scm-not (null? xs)))
         xs) 
        ((test (scm-head xs) (scm-head ys))
         (scm-cons (scm-head xs) 
               (+merge+ (scm-tail xs) ys test)))
        (else 
         (scm-cons (scm-head ys) (+merge+ xs (scm-tail ys) test)))))

(define (+split+ xs)
  (letrec ((split_helper 
            (lambda (xs ys zs) 
              (cond ((null? xs) 
                     (scm-cons ys zs))
                    ((eqv? (scm-length xs) 1) 
                     (scm-cons (scm-cons (scm-head xs) ys) zs))
                    (else (split_helper 
                           (scm-tail (scm-tail xs)) 
                           (scm-cons (scm-head xs) ys) 
                           (scm-cons (scm-head (scm-tail xs)) zs)))))))
    (split_helper xs '() '())))

(define (mergesort xs #!optional (test lt-compare))
  (if (or (null? xs) (eqv? (scm-length xs) 1)) xs 
      (let ((parts (+split+ xs))) 
        (+merge+ (mergesort (scm-head parts) test) 
                 (mergesort (scm-tail parts) test) test))))

(define (scm-sort ls #!optional (test lt-compare) (type 'quick))
  (case type
    ((quick) (quicksort ls test))
    ((merge) (mergesort ls test))
    (else (scm-error "sorting algorithm not implemented" type))))

(define sort scm-sort)
;; 

(define (copy_list lst)
  (if (scm-not (pair? lst))
      (scm-error "(argument 1) PAIR expected" lst))
  (if (scm-not (list? lst))
      (scm-cons (scm-car lst) (scm-cdr lst))
      (let loop ((lst lst) (result '()))
        (cond ((null? lst) (scm-reverse result))
              ((atom? lst) (scm-reverse (scm-cons lst result)))
              (else (loop (scm-cdr lst) (scm-cons (scm-car lst) result)))))))

(define (list_of fill-with n)
  (let ((opr (if (scm-< n 0) + -)))
    (let loop ((r '())
               (n n))
      (if (zero? n) r
          (loop (scm-cons fill-with r) (opr n 1))))))

(define list_tail list-tail)

(define (partition predic ls)
  (let loop ((ls ls) (first '()) (second '()))
    (cond ((null? ls) (scm-cons (scm-reverse first) (scm-reverse second)))
          ((predic (scm-car ls)) (loop (scm-cdr ls) (scm-cons (scm-car ls) first) second))
          (else (loop (scm-cdr ls) first (scm-cons (scm-car ls) second))))))

(define (find predic ls #!key default)
  (let loop ((ls ls))
    (cond ((null? ls) default)
          ((predic (scm-car ls)) (scm-car ls))
          (else (loop (scm-cdr ls))))))

(define (position obj ls #!key (start 0) (test eq?))
  (let loop ((ls ls)
	     (pos 0))
    (cond ((null? ls) #f)
	  ((and (scm->= pos start)
		(test obj (scm-car ls))) 
	   pos)
	  (else (loop (scm-cdr ls) 
		      (scm-+ pos 1))))))

;; defined in extn.slgn.scm
(define sublist scm-sublist)

(define (assp predic ls #!key default)
  (let loop ((ls ls))
    (cond ((null? ls) default)
          ((predic (scm-caar ls)) (scm-car ls))
          (else (loop (scm-cdr ls))))))

(define (zip a b)
  (let loop ((a a) (b b) (result '()))
    (if (or (null? a) (null? b))
        (scm-reverse result)
        (loop (scm-cdr a) (scm-cdr b) (scm-cons (scm-cons (scm-car a) (scm-car b)) result)))))

(define (unzip list-of-pairs)
  (let loop ((xs list-of-pairs) (as '()) (bs '()))
    (if (null? xs)
        (scm-cons (scm-reverse as) (scm-reverse bs))
        (let ((x (scm-car xs)))
          (loop (scm-cdr xs) (scm-cons (scm-car x) as) (scm-cons (scm-cdr x) bs))))))

(define (zip_with f a b)
  (let loop ((a a) (b b) (result '()))
    (if (or (null? a) (null? b))
        (scm-reverse result)
        (loop (scm-cdr a) (scm-cdr b) (scm-cons (f (scm-car a) (scm-car b)) result)))))

(define (exists f ls . more)
  (if (scm-not (null? more)) 
      (assert-equal-lengths ls more))
  (and (scm-not (null? ls))
       (let exists ((x (scm-car ls)) (ls (scm-cdr ls)) (more more))
         (if (null? ls)
             (scm-apply f x (scm-map scm-car more))
             (or (scm-apply f x (scm-map scm-car more))
                 (exists (scm-car ls) (scm-cdr ls) (scm-map scm-cdr more)))))))

(define (for_all f ls . more)
  (if (scm-not (null? more)) 
      (assert-equal-lengths ls more))
  (if (null? ls)
      #f
      (let for-all ((x (scm-car ls)) (ls (scm-cdr ls)) (more more))
        (if (null? ls)
            (scm-apply f x (scm-map scm-car more))
            (and (scm-apply f x (scm-map scm-car more))
                 (for-all (scm-car ls) (scm-cdr ls) (scm-map scm-cdr more)))))))

(define (fold f obj ls . more)
  (if (scm-not (null? more))
      (assert-equal-lengths ls more))
  (let fold-left ((obj obj) (ls ls) (more more))
    (if (null? ls) obj
        (fold-left (scm-apply f obj (scm-car ls) (scm-map scm-car more))
                   (scm-cdr ls) 
                   (scm-map scm-cdr more)))))

(define (fold_left f obj ls . more)
  (if (scm-not (null? more))
      (assert-equal-lengths ls more))
  (let fold-left ((obj obj) (ls ls) (more more))
    (if (null? ls) obj
        (fold-left (scm-apply f (scm-car ls) (scm-append (scm-map scm-car more) (scm-list obj)))
                   (scm-cdr ls) 
                   (scm-map scm-cdr more)))))


;; Taken from http://srfi.schemers.org/srfi-1/srfi-1-reference.scm:
(define (%cdrs lists)
  (call-with-current-continuation
    (lambda (abort)
      (let recur ((lists lists))
	(if (pair? lists)
	    (let ((lis (scm-car lists)))
	      (if (null? lis) (abort '())
		  (scm-cons (scm-cdr lis) (recur (scm-cdr lists)))))
	    '())))))

(define (%cars+ lists last-elt)	; (scm-append! (map car lists) (scm-list last-elt))
  (let recur ((lists lists))
    (if (pair? lists) (scm-cons (scm-caar lists) (recur (scm-cdr lists))) (scm-list last-elt))))

(define (fold_right f obj ls . more)
  (if (scm-not (null? more))
      (assert-equal-lengths ls more))
  (if (pair? more)
      (let recur ((lists (scm-cons ls more)))
	(let ((cdrs (%cdrs lists)))
	  (if (null? cdrs) obj
	      (scm-apply f (%cars+ lists (recur cdrs))))))
      (let recur ((ls ls))
	(if (null? ls) obj
            (f (scm-car ls) (recur (scm-cdr ls)))))))
;; :~

(define (range start end #!key (next scm-inc) (compare <=))
  (let iter ((start start)
             (result (scm-list)))
    (if (compare start end)
        (let ((elem (next start)))
          (iter elem (scm-cons start result)))
        (scm-reverse result))))

(define (find-and-call findf xss f 
                       #!optional call_if_not_found 
                       default)
  (let loop ((xss xss))
    (if (null? xss)
        (if call_if_not_found
            (call_if_not_found)
            (f default))
        (let ((v (findf (scm-car xss))))
          (if v (f v) 
              (loop (scm-cdr xss)))))))

(define (object->list obj)
  (cond ((or (list? obj) (pair? obj) (null? obj))
         obj)
        ((vector? obj)
         (vector->list obj))
        ((hashtable? obj)
         (hashtable->list obj))
        ((set-type? obj)
         (set->list obj))
        ((%bitvector? obj)
         (bitvector->list obj))
        ((string? obj)
         (string->list obj))
        ((s8vector? obj)
         (s8vector->list obj))
        ((u8vector? obj)
         (u8vector->list obj))
        ((s16vector? obj)
         (s16vector->list obj))
        ((u16vector? obj)
         (u16vector->list obj))
        ((s32vector? obj)
         (s32vector->list obj))
        ((u32vector? obj)
         (u32vector->list obj))
        ((s64vector? obj)
         (s64vector->list obj))
        ((u64vector? obj)
         (u64vector->list obj))
        ((f32vector? obj)
         (f32vector->list obj))
        ((f64vector? obj)
         (f64vector->list obj))
        (else (scm-error "cannot convert object to list" obj))))

(define (expand-multiple-comprehension-bindings vars expr)
  (let loop ((vars vars) (bindings '()))
    (if (null? vars)
        `(let* ,bindings ,expr)
        (loop (scm-cdr vars)
              (scm-append bindings `((,(scm-car vars) (scm-car *list-val*))
                                     (*list-val* (scm-cdr *list-val*))))))))

(define (expand-comprehension-binding var expr)
  (cond ((symbol? var)
         `(let ((,var (scm-first *list*))) ,expr))
        ((list? var)
         (cond ((eq? 'scm-cons (scm-car var))
                `(let ((,(scm-cadr var) (scm-caar *list*))
                       (,(scm-caddr var) (scm-cdar *list*)))
                   ,expr))
               ((eq? 'scm-list (scm-car var))
                `(let ((*list-val* (object->list (scm-car *list*))))
                   ,(expand-multiple-comprehension-bindings (scm-cdr var) expr)))
               (scm-error "invalid binding pattern")))
        (else (scm-error "invalid variable syntax"))))

(define (mk-comprehension-loop lists vars filters result-expr)
  (let ((expr-acc '()))
    (if (null? lists) 
        (scm-append 
         expr-acc 
         `(set! *comprehension-result* 
                (scm-cons ,result-expr *comprehension-result*)))
        (scm-append 
         expr-acc 
         `(let *comprehension-loop* ((*list* ,(scm-first lists)))
            (if (scm-not (null? *list*))
                ,(expand-comprehension-binding
                  (scm-first vars)
                  `(begin (if ,(scm-first filters)
                              ,(mk-comprehension-loop
                                (rest-helper lists) (rest-helper vars)
                                (rest-helper filters) result-expr))
                          (*comprehension-loop* (rest-helper *list*))))))))))

(define (list-comprehension lists vars filters result-expr)
  `(let ((*comprehension-result* (scm-list)))
     ,(mk-comprehension-loop lists vars filters result-expr)
     (scm-reverse *comprehension-result*)))
