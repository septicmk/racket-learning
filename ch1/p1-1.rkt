#lang sicp

;;1-1
"##### 1-1 #####"
10
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(/ 7 2)
(+ (* 2 4) (- 4 6))
(define a 3)
(define b ( + a 1))
(+ a b (* a b))
(= a b)
(if (and (> b a) (< b (* a b)))
  b
  a)
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))

(+ 2 (if (> b a) b a))
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))

;;1-2
"##### 1-2 #####"
(/ (+ 5 
      4 
      ( - 2 (- 3 (+ 6 (/ 4 5))))) 
   (* 3 
      (- 6 2) 
      (- 2 7)))

;;1-3
"##### 1-3 #####"
(define (bigger x y)
  (if (> x y) 
    x
    y ))

(define (smaller x y)
  (if (< x y)
    x
    y))

(define (** x)
  (* x x))

(define (sum-of-bigger-two a b c)
  (+ (** (bigger a b))
     (** (bigger c (smaller a b)))))

(sum-of-bigger-two 1 2 3)
(sum-of-bigger-two 2 1 3)
(sum-of-bigger-two 2 3 1)

"##### 1-4 #####"
(define (a-plus-abs-b a b)
  ( (if ( > b 0 ) + - ) a b))

(a-plus-abs-b 1 2)
(a-plus-abs-b 1 -2)


"##### 1-5 #####"
(define (p) (p))

(define (test x y)
  (if ( = x 0 )
    0 
    y))

(test 0 p)

"##### 1-6 #####"


(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause )))



(new-if #t (display "good") (display "bad"))
(display "\n")
(if #t (display "good") (display "bad"))
(display "\n")

"##### 1-7 #####"

(define (sqrt-iter guess x)
  ( if (good-enough? guess (improve guess x))
       (improve guess x)
       (sqrt-iter (improve guess x) x)))

(define (good-enough? last-guess guess)
  (< (abs (- last-guess guess)) 0.001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y ) 2))

(define (my-sqrt x)
  (sqrt-iter 1. x))

(my-sqrt 9)

"##### 1-8 #####"

(define (cube-root-iter guess x)
  ( if (good-enough? guess (improve-cube guess x))
       (improve-cube guess x)
       (cube-root-iter (improve-cube guess x) x)))

(define (improve-cube y x)
  (/ (+ (/ x (* y y)) 
        (* 2 y)) 
     3))

(define (cube-root x)
  (cube-root-iter 1. x))

(define (cube x)
  (* x x x))

(cube-root (cube 4))
(cube-root (cube 2.7))
(cube-root (cube 1))



