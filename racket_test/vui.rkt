 #lang racket/gui

;;;
;;; WORLD
;;;

(define-struct world (lines))
(define the-world (make-world '((0 . 0) (0 . 300) (250 . 250) (150 . 176) (10 . 4) (280 . 10))))

;;;
;;; USER LAND
;;;

(define (on-mouse-event world event)
  (if (and (send event get-left-down)
           (send event moving?)
           #; (send event button-changed?))
      (let ((x (send event get-x))
            (y (send event get-y)))
        (make-world (cons (cons x y) (world-lines world))))
      world))

(define (on-paint world dc)  
  (send dc draw-lines 
        (map pair->point (world-lines world))))

(define (pair->point p)
  (make-object point% (car p) (cdr p)))


;;;
;;; SYSTEM
;;;

(define user:on-paint on-paint)

(define diagramframe (new frame% [label "paint"] [width 300] [height 300] [x 1000][y 300]))

(define paintcanvas% 
  (class canvas%
    (inherit get-dc refresh)
    (super-new)

    (define/override (on-paint)
      (send (get-dc) suspend-flush)
      (user:on-paint the-world (get-dc))
      (send (get-dc) resume-flush))

    (define/override (on-event mouse-event)
      (let* ([old-world the-world]
             [new-world (on-mouse-event the-world mouse-event)])
        (if (eq? old-world new-world)
            (super on-event mouse-event)
            (begin
              (set! the-world new-world)
              (refresh)))))))

(define paintcanvas (new paintcanvas% [parent diagramframe]))
(send diagramframe show #t)
