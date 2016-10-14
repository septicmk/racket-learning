#lang typed/racket

(define-type (CatListof A) (U (leaf A) (cat A)))

(struct (A) leaf ([a : A]))
(struct (A) cat ([l : (CatListof A)]
                 [r : (CatListof A)]))

(: cat-map (All (A B) (-> (-> A B) (CatListof A) (CatListof B))))
(define (cat-map f xs)
  (match xs
  [(leaf x) (leaf (f x))]
  [(cat l r) (cat (cat-map f l)
                  (cat-map f r))]))

(: in-parallel (All (A B C) (-> (-> A B) (-> A C) A (Pairof B C))))
(define (in-parallel f g x)
  (let ([fx (future (lambda () (f x)))]
        [gx (g x)])
    (cons (touch fx) gx)))

(: par-map (All (A B) (-> (-> A B) (CatListof A) (CatListof B))))
(define (par-map f xs)
  (match xs
    [(leaf x) (leaf (f x))]
    [(cat l r)
     (let ([fl (future (lambda () (par-map f l)))]
           [fr (future (lambda () (par-map f r)))])
       (cat (touch fl) (touch fr)))]))

;;;;;;;;;;;;;;;;;;;Rope

;;( define-type ( Ropeof A ) ( U ( leaf A ) ( cat A )))
;;( struct ( A ) leaf ([ as : ( Listof A )]))
;;( struct ( A ) cat ([ l : ( Ropeof A )] [r : ( Ropeof A )]))
;;
;;(: rope-map (All (A B) (-> (-> A B) (Ropeof A) (Ropeof B))))
;;(define (rope-map f rope)
  ;;(match rope
    ;;[(leaf as) (leaf (map f as))]
    ;;[(cat l r) (cat (rope-map f l)
                    ;;(rope-map f r))]))
;;
;;(: rope-pmap (All (A B) (-> (-> A B) (Ropeof A) (Ropeof B))))
;;(define (rope-pmap f rope)
  ;;(match rope
    ;;[(leaf as) (leaf (map f as))]
    ;;[(cat l r) (let ([fl (future (lambda () (rope-pmap f l)))]
                     ;;[fr (future (lambda () (rope-pmap f r)))])
                 ;;(cat (touch fl) (touch fr)))]))
;;
;;
;;(: rope-reduce (All (A) (-> (-> A A A) (Ropeof A) A)))
;;(define (rope-reduce f rope)
  ;;(match rope
    ;;[(leaf as) (list-reduce f as)]
    ;;[(cat l r) (f (rope-reduce f l)
                  ;;(rope-reduce f r))]))
;;
;;(: rope-preduce (All (A) (-> (-> A A A) (Ropeof A) A)))
;;(define (rope-preduce f rope)
  ;;(match rope
    ;;[(leaf as) (list-reduce f as)]
    ;;[(cat l r) (let ([fl (future (lambda () (rope-preduce f l)))]
                     ;;[fr (future (lambda () (rope-precude f r)))])
                 ;;(f (touch fl) (touch fr)))]))
;;
;;
