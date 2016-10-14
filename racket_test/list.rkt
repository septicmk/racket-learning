#lang typed/racket

(cons 1 `())


(struct Nil ())
(struct (A) link ([ head : A ] [ tail : ( LinkedList A )]))
(define-type ( LinkedList A ) ( U Nil ( link A )))

(link `a (link `b (link `c (Nil))))


(: list-length (All (A) (-> (LinkedList A) Integer)))
(define (list-length as)
  (match as
  [(Nil) 0]
  [(link _ tail) (+ 1 (list-length tail))]))


(: list-push (All (A) (-> (LinkedList A) A (LinkedList A))))
(define (list-push as e)
  (link e as))


(: list-concat (All (A) (-> (LinkedList A) (LinkedList A) (LinkedList A))))
(define (list-concat lhs rhs)
  (match lhs
  [(Nil) rhs]
  [(link head tail) (list-push (list-concat tail rhs) head)]))

(: list-remove (All (A) (-> (LinkedList A) A (LinkedList A))))
(define (list-remove as e)
  (match as
    [(Nil) (Nil)]
    [(link head tail)
     (if (eq? e head)
       tail
       (link head (list-remove tail e)))]))


(: list-map ( All (A B) (-> (-> A B) (LinkedList A) (LinkedList B))))
(define (list-map f as)
  (match as
    [(Nil) (Nil)]
    [(link head tail)
     (link (f head) (list-map f tail))]))



