#lang racket

(require 2htdp/image 2htdp/universe lens gregor gregor/period)
(require (except-in racket/gui make-pen make-color date date?))

;; TODO
;; - powerset
;; - draw board
;; - draw coins
;; - calculate square
;; - choose square (using mouse)
;; - tell user wether they chose the right square or not

(define-values (EASY MEDIUM HARD) (values 1 2 3))
(define DIFFICULTY 'to-be-decided-by-the-user)
(define GAME-ROUNDS 3)
(define (set-board-size)
(define BOARD-SIZE (expt 2 DIFFICULTY))
(define SQUARE-SIZE 50)
(define COIN-SIZE 15)
(define BOARD-WIDTH (* BOARD-SIZE SQUARE-SIZE))
(define EMPTY-SCENE (empty-scene (+ 100 BOARD-WIDTH) (+ 100 BOARD-WIDTH)))
(define TRANSPARENT (color 0 0 0 0))
(define SQUARE-COLOR1 (color 200 200 200))
(define SQUARE-COLOR2 (color 50 50 50))
(define COIN-HEADS-COLOR "gold")
(define COIN-TAILS-COLOR TRANSPARENT)
(define POWER-SET-SIZE (inexact->exact (log (sqr BOARD-SIZE) 2)))
(define BASIC-SET (build-list POWER-SET-SIZE identity))

(define HIGHLIGHT-SQUARE (square SQUARE-SIZE "solid" (color 0 255 0 50)))
(define BLANK-SQUARE (square SQUARE-SIZE "solid" TRANSPARENT))
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

(define (take-half l) (take l (/ (length l) 2)))
(define (drop-half l) (drop l (/ (length l) 2)))

(define (make-coin heads)
  (freeze (place-image
           (circle COIN-SIZE "solid" (if heads COIN-HEADS-COLOR COIN-TAILS-COLOR))
           (/ SQUARE-SIZE 2) (/ SQUARE-SIZE 2)
           BLANK-SQUARE)))

(define (make-square position)
  (square SQUARE-SIZE "solid" (if (equal? (modulo (+ (car position) (cdr position)) 2) 0)
                                  SQUARE-COLOR1 SQUARE-COLOR2)))

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
              (draw-image-on-board (make-square position) position img)))]))
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


(define (hash-addset h s)
  (cond [(empty? s) h]
        [(hash-has-key? h (first s))
         (hash-addset (hash-set h (first s) (modulo (add1 (hash-ref h (first s))) 2)) (rest s))]
        [else (hash-addset (hash-set h (first s) 1) (rest s))]))

(define (build-hash coins ps h)
  (cond [(empty? coins) h]
        [(zero? (first coins)) (build-hash (rest coins) (rest ps) (hash-addset h (first ps)))]
        [else (build-hash (rest coins) (rest ps) h)]))


(define (which-square-helper h s p)
  (cond [(empty? s) (first p)]
        [(or (not (hash-has-key? h (first s))) (zero? (hash-ref h (first s))))
         (which-square-helper h (rest s) (take-half p))]
        [else (which-square-helper h (rest s) (drop-half p))]))

(define (which-square h)
  (let ([horizontal (which-square-helper h (take-half BASIC-SET) (build-list BOARD-SIZE identity))]
        [vertical (which-square-helper h (drop-half BASIC-SET) (build-list BOARD-SIZE identity))])
    (+ (* vertical BOARD-SIZE) horizontal)))


;; USEFUL CODE SNIPPETS FOR LATER XD

;COINS
;PS
;(define my-hash (build-hash COINS PS (make-immutable-hash)))

;my-hash


;(which-square my-hash)
;(fill-board 0 COINS EMPTY-BOARD)

;; GETTING TIME
;(define a (period->list (time-period-between (now) (now))))
;(define h (make-immutable-hash a))
;(hash-ref h 'hours)

(define (elapsed-time then)
  (let ([h (make-immutable-hash (period->list (time-period-between then (now))))])
    (string-append
     (number->string (hash-ref h 'minutes)) ":" (number->string (hash-ref h 'seconds)))))

(define EMPTY-BOARD (draw-board BOARD-SIZE))
(define PS (powerset BASIC-SET))
(struct/lens game [board round correct-square start-time finished-rounds] #:transparent)

(define (start-game)
  (let* ([coins (flip-coins (sqr BOARD-SIZE))]
         [correct-square (which-square (build-hash coins PS (make-immutable-hash)))])
    (game (fill-board 0 coins EMPTY-BOARD) 1 correct-square (now) empty)))

(define (render-game g)
  (place-image/align
   (text (elapsed-time (game-start-time g)) 12 "black") (+ 20 BOARD-WIDTH) 20 "left" "top"
   (place-image/align (game-board g) 0 0 "left" "top" EMPTY-SCENE)))

(define (handle-mouse g x y m)
  (println x)
  (println y)
  (println m) g)

(define (game-over? g)
  (> (game-round g) GAME-ROUNDS))

(define (update-game g) g)

(define (start)
  (big-bang (start-game)
    (on-tick update-game (/ 1 10)) ;don't need high tick rate, just updating time
    (on-mouse handle-mouse)
    (to-draw render-game)
    (stop-when game-over? render-game) ;(stop-when (λ (x) #f) render-game)
    (name "Coin Guesser")
    (close-on-stop 1)))

;; TODO: write macro to improve syntax here, this is too wordy should be able to write as
;;
;; (frame% fame ([label "Coind Guesser"])
;;   (vertical-panel% choose-difficult-pannel ()
;;     (verical-pane% vert-pane ()
;;       (message% msg ([label "Choose Difficulty:"]))
;;       (message% diff ([label ""])))
;;     (horizontal-pane% button-pane ([alignment '(center top)])
;;       (button% ([label "Easy"]
;;                 [callback (lambda (button event) (send diff set-lable "EASY))]))...



(define frame (new frame% [label "Coin Guesser"]))

(define choose-difficulty-panel (new vertical-panel% [parent frame]))
(define vert-pane (new vertical-pane% [parent choose-difficulty-panel]))
(define button-pane (new horizontal-pane%
                         [parent choose-difficulty-panel]
                         [alignment '(center top)]))

(define msg (new message%
                 [parent vert-pane]
                 [label "Choose Difficulty:"]))
;(define diff (new message%
;                  [parent vert-pane]
;                  [label ""]
;                  [auto-resize #t]))

(define (start-game difficulty)
  (set! DIFFICULTY difficulty)
  (send choose-difficulty-panel is-shown #f)
  (send game-panel is-shown #t))

(new button%
     [parent button-pane]
     [label "Easy"]
     [callback (lambda (button event) (start-game EASY))])
(new button%
     [parent button-pane]
     [label "Medium"]
     [callback (lambda (button event) (start-game MEDIUM))])
(new button%
     [parent button-pane]
     [label "Hard"]
     [callback (lambda (button event) (start-game HARD))])
(send frame fullscreen #f)
(send frame show #t)












    






