#lang typed/racket

(struct None ())
(struct (A) Some ([a : A]))
;; U equal union
(define-type (Maybe A) (U None (Some A)))

(: maybe-divide (-> Number Number (Maybe Number)))
(define (maybe-divide n d)
  (if (= d 0)
    (None)
    (Some (/ n d))))

(: has-value? (-> (Maybe Any) Boolean))
(define (has-value? m)
  (match m
    [(None) #f]
    [(Some _) #t]))

;; what the fuck????
(: maybe-map (All (A B) (-> (-> A B) (Maybe A) (Maybe B))))
(define (maybe-map f m)
  (match m
    [(None) (None)]
    [(Some a) (Some (f a))]))

(maybe-map (lambda ([x : Number]) (* 2 x))
           (maybe-divide 42 23))

