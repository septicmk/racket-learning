#lang typed/racket

(require "rope-parallel.rkt")

(define ropeA (rope-init-interval 1 10))
(define ropeB (rope-init-interval 11 20))
ropeA
ropeB


(rope-map-reduce (lambda ([x : Integer]) (* 2 x)) (lambda ([x : Integer] [y : Integer]) (+ x y)) ropeA)
(p-rope-map-reduce (lambda ([x : Integer]) (* 2 x)) (lambda ([x : Integer] [y : Integer]) (+ x y)) ropeA)

(rope-preverse ropeA)

(rope-back-reduce (lambda ([x : Integer] [y : Integer]) (+ x y)) ropeA)

(rope-zip-with (lambda ([x : Integer] [y : Integer]) (+ x y)) ropeA ropeB)


(rope-for-all (lambda ([x : Integer]) (positive? x)) ropeA)
(p-rope-for-all (lambda ([x : Integer]) (> x 5)) ropeA)
(p-rope-exists (lambda ([x : Integer]) (> x 5)) ropeA)

(rope-scan (lambda ([x : Integer] [y : Integer]) (+ x y)) 0 ropeA)

