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
