#lang racket

(require 2htdp/image)

;; TODO
;; - powerset
;; - draw board
;; - draw coins
;; - calculate square
;; - choose square (using mouse)
;; - tell user wether they chose the right square or not

(define BOARD-SIZE 4)
(define SQUARE-SIZE 50)
(define COIN-SIZE 15)
(define SQUARE-COLOR1 (color 200 200 200))
(define SQUARE-COLOR2 (color 50 50 50))
(define COIN-COLOR "gold")

(define (scalev v) (* SQUARE-SIZE v))
(define (get-x position) (modulo position BOARD-SIZE))
(define (get-y position) (inexact->exact (floor (/ position BOARD-SIZE))))
(define (get-xy position) (cons (get-x position) (get-y position)))

(define (square-color position)
  (if (equal? (modulo (+ (car position) (cdr position)) 2) 0)
      SQUARE-COLOR1 SQUARE-COLOR2))

(define (draw-image-on-board img position board)
  (place-image/align img (scalev (car position)) (scalev (cdr position)) "left" "top" board))

(define (draw-board)
  (define (draw-squares counter img)
    (cond [(equal? counter (sqr BOARD-SIZE)) img]
          [else
           (let ([position (get-xy counter)])
             (draw-squares (add1 counter) (draw-image-on-board
                                           (square SQUARE-SIZE "solid" (square-color position)) position img)))]))
  (draw-squares 0 (empty-scene (scalev BOARD-SIZE) (scalev BOARD-SIZE))))

(define (powerset s)
  (cond [(empty? s) '(())]
        [else (let ([rest-powerset (powerset (rest s))])
                (append rest-powerset
                        (map (λ (l) (cons (first s) l)) rest-powerset)))]))

(define EMPTY-BOARD (freeze (draw-board)))
(define COIN (freeze (place-image
                      (circle COIN-SIZE "solid" COIN-COLOR)
                      (/ SQUARE-SIZE 2) (/ SQUARE-SIZE 2)
                      (square SQUARE-SIZE "solid" (color 0 0 0 0)))))




    






