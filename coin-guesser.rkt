#lang racket

(require 2htdp/image 2htdp/universe lens gregor gregor/period) ;; TODO get rid of gregor dependencies
(require (except-in racket/gui make-pen make-color date date?))

(define board%
  (class object%
    (super-new)
    ;; INIT
    (init difficulty)

    ;; CONSTANTS
    (define GAME-ROUNDS 3)
    (define SQUARE-SIZE 50)
    (define COIN-SIZE 15)
    (define TRANSPARENT (color 0 0 0 0))
    (define SQUARE-COLOR1 (color 200 200 200))
    (define SQUARE-COLOR2 (color 50 50 50))
    (define COIN-HEADS-COLOR "gold")
    (define COIN-TAILS-COLOR TRANSPARENT)

    ;; FINAL MEMBERS
    (define board-size (expt 2 difficulty))
    (define board-width (* board-size SQUARE-SIZE))
    (define power-set-size (inexact->exact (log (sqr board-size) 2)))
    (define basic-set (build-list power-set-size identity))

    ;; RENDERABLE CONSTANTS
    (define GREEN-SQUARE (square SQUARE-SIZE "solid" (color 0 255 0 50)))
    (define BLANK-SQUARE (square SQUARE-SIZE "solid" TRANSPARENT))
    (define HEADS (freeze (place-image
                           (circle COIN-SIZE "solid" COIN-HEADS-COLOR)
                           (/ SQUARE-SIZE 2) (/ SQUARE-SIZE 2)
                           BLANK-SQUARE)))
    (define TAILS (freeze (place-image
                           (circle COIN-SIZE "solid" COIN-TAILS-COLOR)
                           (/ SQUARE-SIZE 2) (/ SQUARE-SIZE 2)
                           BLANK-SQUARE)))

    ;; PRIVATE HELPER FUNCTIONS
    (define (scalev v) (* SQUARE-SIZE v))
    (define (get-x position) (modulo position board-size))
    (define (get-y position) (inexact->exact (floor (/ position board-size))))
    (define (get-xy position) (cons (get-x position) (get-y position)))
    (define (take-half l) (take l (/ (length l) 2)))
    (define (drop-half l) (drop l (/ (length l) 2)))

    ;; PRIVATE FUNCTIONS
    (define (make-coin heads)
      (freeze (place-image
               (circle COIN-SIZE "solid" (if heads COIN-HEADS-COLOR COIN-TAILS-COLOR))
               (/ SQUARE-SIZE 2) (/ SQUARE-SIZE 2)
               BLANK-SQUARE)))
    ;; makes a square and decides it's color based on it's position
    (define (make-square position)
      (square SQUARE-SIZE "solid" (if (equal? (modulo (+ (car position) (cdr position)) 2) 0)
                                      SQUARE-COLOR1 SQUARE-COLOR2)))

    (define (flip-coins n)
      (cond [(zero? n) '()]
            [else (cons (random 2) (flip-coins (sub1 n)))]))

    (define (draw-image-on-board img position board)
      (place-image/align img (scalev (car position)) (scalev (cdr position)) "left" "top" board))

    (define (draw-board)
      (define (draw-squares counter img)
        (cond [(equal? counter (sqr board-size)) (freeze img)]
              [else
               (let ([position (get-xy counter)])
                 (draw-squares
                  (add1 counter)
                  (draw-image-on-board (make-square position) position img)))]))
      (draw-squares 0 (empty-scene (scalev board-size) (scalev board-size))))

    (define (powerset s)
      (cond [(empty? s) '(())]
            [else (let ([rest-powerset (powerset (rest s))])
                    (append rest-powerset
                            (map (λ (l) (cons (first s) l)) rest-powerset)))]))

    (define (fill-board counter coins board)
      (cond [(empty? coins) (freeze board)]
            [else (fill-board (add1 counter) (rest coins)
                              (draw-image-on-board (make-coin (zero? (first coins))) (get-xy counter) board))]))

    ;; Function group for calculating correct which square the board is pointing to
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
      (let ([horizontal (which-square-helper h (take-half basic-set) (build-list board-size identity))]
            [vertical (which-square-helper h (drop-half basic-set) (build-list board-size identity))])
        (+ (* vertical board-size) horizontal)))

    ;; 
    (define empty-board (draw-board))
    (define ps (powerset basic-set))
    (define coins (flip-coins (sqr board-size)))
    (define correct-square (which-square (build-hash coins ps (make-immutable-hash))))
    (define plain-game-board (fill-board 0 coins empty-board))
    (define game-board plain-game-board)

    ;; PUBLIC FUNCTIONS
    (define/public (get-bitmap)
      (color-list->bitmap (image->color-list game-board) board-width board-width))))
      
      
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

;(define (elapsed-time then)
;  (let ([h (make-immutable-hash (period->list (time-period-between then (now))))])
;    (string-append
;     (number->string (hash-ref h 'minutes)) ":" (number->string (hash-ref h 'seconds)))))
;
;(define EMPTY-BOARD (draw-board BOARD-SIZE))
;(define PS (powerset BASIC-SET))
;(struct/lens game [board round correct-square start-time finished-rounds] #:transparent)
;
;
;
;(define (render-game g)
;  (place-image/align
;   (text (elapsed-time (game-start-time g)) 12 "black") (+ 20 BOARD-WIDTH) 20 "left" "top"
;   (place-image/align (game-board g) 0 0 "left" "top" EMPTY-SCENE)))
;
;(define (handle-mouse g x y m)
;  (println x)
;  (println y)
;  (println m) g)
;
;(define (game-over? g)
;  (> (game-round g) GAME-ROUNDS))
;
;(define (update-game g) g)
;
;(define (start)
;  (big-bang (start-game)
;    (on-tick update-game (/ 1 10)) ;don't need high tick rate, just updating time
;    (on-mouse handle-mouse)
;    (to-draw render-game)
;    (stop-when game-over? render-game) ;(stop-when (λ (x) #f) render-game)
;    (name "Coin Guesser")
;    (close-on-stop 1)))

;; TODO: write macro to improve syntax here, this is too wordy should be able to write as
;;
;; (frame% fame ([label "Coind Guesser"])
;;   (vertical-panel% choose-difficult-pannel ()
;;     (verical-pane% vert-pane ()
;;       (message% msg ([label "Choose Difficulty:"]))
;;       (message% diff ([label ""])))
;;     (horizontal-pane% button-pane ([alignment '(center top)])
;;       (button% ([label "Easy"]
;;                 [callback (λ (button event) (send diff set-lable "EASY))]))...

(define the-hidden-frame (new frame% [label "If you see this i don't understand what's going on"]))
(define frame (new frame% [label "Coin Guesser"]))

(define current-board empty)
(define (start-game difficulty)
  (set! current-board (new board% [difficulty difficulty]))
  (send choose-difficulty-panel reparent the-hidden-frame)
  (send game-panel reparent frame))

(define game-panel (new panel% [parent the-hidden-frame]))

(define canvas (new canvas%
                    [parent game-panel]
                    [paint-callback
                     (λ (canvas dc)
                       (send dc draw-bitmap (send current-board get-bitmap) 0 0))]))

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

(new button%
     [parent button-pane]
     [label "Easy"]
     [callback (λ (button event) (start-game 1))])
(new button%
     [parent button-pane]
     [label "Medium"]
     [callback (λ (button event) (start-game 2))])
(new button%
     [parent button-pane]
     [label "Hard"]
     [callback (λ (button event) (start-game 3))])
(send frame fullscreen #f)
(send frame show #t)












    






