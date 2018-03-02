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
;(define SQUARE-COLOR1 "purple")
(define SQUARE-COLOR2 (color 50 50 50))
(define COIN-HEADS-COLOR "gold")
;(define COIN-TAILS-COLOR "silver")
(define COIN-TAILS-COLOR (color 0 0 0 0))
(define POWER-SET-SIZE (inexact->exact (log (sqr BOARD-SIZE) 2)))

(define BLANK-SQUARE (square SQUARE-SIZE "solid" (color 0 0 0 0)))
(define HEADS (freeze (place-image
                       (circle COIN-SIZE "solid" COIN-HEADS-COLOR)
                       (/ SQUARE-SIZE 2) (/ SQUARE-SIZE 2)
                       BLANK-SQUARE)))
(define TAILS (freeze (place-image
                       (circle COIN-SIZE "solid" COIN-TAILS-COLOR)
                       (/ SQUARE-SIZE 2) (/ SQUARE-SIZE 2)
                       BLANK-SQUARE)))

(define (scalev v) (* SQUARE-SIZE v))
(define (get-x position) (modulo position BOARD-SIZE))
(define (get-y position) (inexact->exact (floor (/ position BOARD-SIZE))))
(define (get-xy position) (cons (get-x position) (get-y position)))

(define (square-color position)
  (if (equal? (modulo (+ (car position) (cdr position)) 2) 0)
      SQUARE-COLOR1 SQUARE-COLOR2))

(define (make-coin heads)
  (freeze (place-image
           (circle COIN-SIZE "solid" (if heads COIN-HEADS-COLOR COIN-TAILS-COLOR))
           (/ SQUARE-SIZE 2) (/ SQUARE-SIZE 2)
           BLANK-SQUARE)))

(define (flip-coins n)
  (cond [(zero? n) '()]
        [else (cons (random 2) (flip-coins (sub1 n)))]))

(define (draw-image-on-board img position board)
  (place-image/align img (scalev (car position)) (scalev (cdr position)) "left" "top" board))

(define (draw-board size)
  (define (draw-squares counter img)
    (cond [(equal? counter (sqr size)) (freeze img)]
          [else
           (let ([position (get-xy counter)])
             (draw-squares
              (add1 counter)
              (draw-image-on-board
               (square SQUARE-SIZE "solid" (square-color position)) position img)))]))
  (draw-squares 0 (empty-scene (scalev size) (scalev size))))

(define (powerset s)
  (cond [(empty? s) '(())]
        [else (let ([rest-powerset (powerset (rest s))])
                (append rest-powerset
                        (map (λ (l) (cons (first s) l)) rest-powerset)))]))

(define (fill-board counter coins board)
  (cond [(empty? coins) (freeze board)]
        [else (fill-board (add1 counter) (rest coins)
                          (draw-image-on-board (make-coin (zero? (first coins))) (get-xy counter) board))]))
;(fill-board-helper 0 (flip-coins (sqr size)) EMPTY-BOARD))


(define (hash-addset h s)
  (println h)
  (cond [(empty? s) h]
        [(hash-has-key? h (first s))
         (hash-addset (hash-set h (first s) (add1 (hash-ref h (first s)))) (rest s))]
        [else (hash-addset (hash-set h (first s) 1) (rest s))]))

(define (build-hash coins ps h)
  (cond [(empty? coins) h]
        [(zero? (first coins)) (build-hash (rest coins) (rest ps) (hash-addset h (first ps)))]
        [else (build-hash (rest coins) (rest ps) h)]))


(define EMPTY-BOARD (draw-board BOARD-SIZE))
(define COINS (flip-coins (sqr BOARD-SIZE)))
(define PS (powerset (build-list POWER-SET-SIZE identity)))

(fill-board 0 COINS EMPTY-BOARD)
COINS
PS
(build-hash COINS PS (make-immutable-hash))

    






