;;#lang typed/racket
#lang sicp

;; test 1 - define
(define size 2)
(define pi 3.141592653)
(define r 10)
(define cf (* 2 pi r))

;;(* 5 size)
;;cf

;; test 2 - function
(define (square x)
  (* x x))

(define (sum-of-square x y)
  (+ (square x) (square y)))

;;(square 10)
;;(sum-of-square 3 4)


;;test 3 - cond
(define (abs_1 x)
  (cond (( > x 0 ) x)
        (( = x 0 ) 0)
        (( < x 0 ) (- x))))

(define (abs_2 x)
  (cond (( < x 0) (- x))
        (else x)))

(define (abs_3 x)
  (if ( < x 0 ) (- x) x))

;; test 3 - guess sqrt

(define (sqrt-iter guess x)
  ( if (good-enough? guess x)
       guess
       (sqrt-iter (improve guess x) x)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y ) 2))

(define (my-sqrt x)
  (sqrt-iter 1. x))

;;(my-sqrt 9)
