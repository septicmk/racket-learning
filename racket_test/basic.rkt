#lang typed/racket

;;#lang racket
;;comment
(print "hello world!")

;; define (f arg1 arg2 ... )
(define x (* (+ 1 2) (+ 3 4)))
x
(print x)

;; local variables; you can never change them
(let ([x (* 3 4)])
  (print x))

;; function
(: times-two (-> Number Number)) 
;; typed/racket only (-> A B) means A -> B
;;(-> A B C D) means A B C -> D
(define (times-two x)
  (* 2 x))

;; more complex function
(: duel ( All (A) (-> A (Pairof A A) ) ) )
(define (duel x)
  (cons x x) )
(print (duel 12))

;; recursion
(: is-even? (-> Integer Boolean))
(define (is-even? x)
  (if (> x 1) 
    (is-even? (- x 2))
    (= x 0)))
( print ( is-even? 11 ) )


;; pattern match
(: is-even?2 (-> Integer Boolean))
(define (is-even?2 x)
  ( match x
          [0 #t]
          [1 #f]
          [_ (is-even?2 (- x 2))] ))
( print ( is-even? 123 ) )

;; lambda expressions
((lambda (x) (* x 2)) 2)

(struct myNumberBox ([v : Number]))
;; struct
(struct point 
        ([x : Integer]
         [y : Integer]) )

;; polymorphism/meta/templeate
(struct (A) anyElem 
              ([x : A]))

;; access field
(define p (point 1 2))
(point-x p)
