#lang racket

(require racket/gui)
;(require (except-in racket/gui make-pen make-color date date?))

(define board%
  (class object%
    (super-new)
    ;; INIT
    (init difficulty)

    ;; CONSTANTS
    (define GAME-ROUNDS 3)
    (define SQUARE-SIZE 80)
    (define COIN-SIZE 50)

    (define TRANSPARENT (make-object color% 0 0 0 0))
    (define SQUARE-COLOR1 (make-object color% 200 200 200))
    (define SQUARE-COLOR2 (make-object color% 50 50 50))
    (define COIN-HEADS-COLOR (make-object color% "Firebrick"))
    (define CORRECT-COLOR (make-object color% 0 255 0 0.35))
    (define WRONG-COLOR (make-object color% 255 0 0 0.35))
    (define HOVER-COLOR (make-object color% 150 150 255 0.35))
    (define PRESS-COLOR (make-object color% 150 150 255 0.7))
    (define COIN-TAILS-COLOR TRANSPARENT)

    ;; FINAL MEMBERS
    (define coin-offset (/ (- SQUARE-SIZE COIN-SIZE) 2))
    (define board-size (expt 2 difficulty))
    (define board-width (* board-size SQUARE-SIZE))
    (define power-set-size (inexact->exact (log (sqr board-size) 2)))
    (define basic-set (build-list power-set-size identity))

    ;; RENDERABLE CONSTANTS
    (define (draw-square dc position color)
      (send dc set-pen (make-object pen% TRANSPARENT 0))
      (send dc set-brush (make-object brush% color 'solid))
      (send dc draw-rectangle (scalev (car position)) (scalev (cdr position)) SQUARE-SIZE SQUARE-SIZE))

    (define (draw-circle dc position color)
      (send dc set-smoothing 'aligned)
      (send dc set-pen (make-object pen% TRANSPARENT 0))
      (send dc set-brush (make-object brush% color 'solid))
      (send dc draw-arc
            (+ coin-offset (scalev (car position)))
            (+ coin-offset (scalev (cdr position)))
            COIN-SIZE COIN-SIZE 0 (* 2 pi)))
      
    ;; PRIVATE HELPER FUNCTIONS
    (define (scalev v) (* SQUARE-SIZE v))
    (define (get-x position) (modulo position board-size))
    (define (get-y position) (inexact->exact (floor (/ position board-size))))
    (define (get-xy position) (cons (get-x position) (get-y position)))
    (define (take-half l) (take l (/ (length l) 2)))
    (define (drop-half l) (drop l (/ (length l) 2)))

    (define (xy->position x y)
      (cons
       (inexact->exact (floor (/ x SQUARE-SIZE)))
       (inexact->exact (floor (/ y SQUARE-SIZE)))))

      (define (flip-coins n)
        (cond [(zero? n) '()]
              [else (cons (random 2) (flip-coins (sub1 n)))]))

      (define (decide-square-color position)
        (if (equal? (modulo (+ (car position) (cdr position)) 2) 0) SQUARE-COLOR1 SQUARE-COLOR2))

      (define (powerset s)
        (cond [(empty? s) '(())]
              [else (let ([rest-powerset (powerset (rest s))])
                      (append rest-powerset
                              (map (λ (l) (cons (first s) l)) rest-powerset)))]))

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

      (define (draw-board)
        (define (draw-squares counter board-bmdc)
          (cond [(equal? counter (sqr board-size)) (send board-bmdc get-bitmap)]
                [else
                 (let ([position (get-xy counter)])
                   (draw-square board-bmdc position (decide-square-color position))
                   (draw-squares (add1 counter) board-bmdc))]))
        (draw-squares 0 (make-object bitmap-dc% (make-object bitmap% board-width board-width))))

      (define (fill-board counter coins board-bmdc)
        (cond [(empty? coins) (send board-bmdc get-bitmap)]
              [else
               (draw-circle board-bmdc (get-xy counter) (if (zero? (first coins)) COIN-HEADS-COLOR COIN-TAILS-COLOR))
               (fill-board (add1 counter) (rest coins) board-bmdc)]))

      (define (highlight-square board-bmdc position color)
        (draw-square board-bmdc position color)
        (send board-bmdc get-bitmap))

      ;; 
      (define empty-board (draw-board))
      (define ps (powerset basic-set))
      (define coins (flip-coins (sqr board-size)))
      (define correct-square (which-square (build-hash coins ps (make-immutable-hash))))
      (define plain-game-board (fill-board 0 coins (make-object bitmap-dc% empty-board)))
      (define board-bitmap plain-game-board)
      (define revealed #f)

      (define (get-board-bmdc)
        (define new-bmdc (make-object bitmap-dc% (make-object bitmap% board-width board-width)))
        (send new-bmdc draw-bitmap plain-game-board 0 0)
        new-bmdc)

     (define (try-square position)
       (let ([board-bdmc (get-board-bmdc)]
             [correct-position (get-xy correct-square)])
          (if (not (equal? correct-position position))
              (set! plain-game-board (highlight-square (get-board-bmdc) position WRONG-COLOR)) empty)
          (set! plain-game-board (highlight-square (get-board-bmdc) correct-position CORRECT-COLOR))
          (set! board-bitmap plain-game-board)
          (set! revealed #true)
          (send choose-difficulty-panel reparent game-panel-side)))

      ;; PUBLIC FUNCTIONS

      (define/public (handle-mouse mouse-event)
        (let* ([event-type (send mouse-event get-event-type)]
               [mouse-x (send mouse-event get-x)]
               [mouse-y (send mouse-event get-y)]
               [position (xy->position mouse-x mouse-y)])
          (cond [revealed empty]
                [(equal? event-type 'left-up) (try-square position)]
                [(equal? event-type 'left-down) (set! board-bitmap (highlight-square (get-board-bmdc) position PRESS-COLOR))]
                [(equal? event-type 'leave) (set! board-bitmap plain-game-board)]
                [(equal? event-type 'motion) (set! board-bitmap (highlight-square (get-board-bmdc) position HOVER-COLOR))])))

      (define/public (get-bitmap) board-bitmap)))
      
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

(define current-board empty)
(define the-hidden-frame (new frame% [label "If you see this i don't understand what's going on"]))
(define frame (new frame% [label "Coin Guesser"]))

(define game-panel (new horizontal-panel%
                        [parent the-hidden-frame]))

;;TODO: it's kinda weird the board-canvas uses current-board
(define board-canvas%
  (class canvas%
    (inherit get-width get-height refresh)
    
    (define/override (on-event event)
      (if (is-a? event mouse-event%) (send current-board handle-mouse event) (println "not a mouse event"))
      (refresh))

    (define/private (my-paint-callback self dc)
      (let ([bm (send current-board get-bitmap)])
        (send dc draw-bitmap bm 0 0)
        (send canvas min-width (send bm get-width))
        (send canvas min-height (send bm get-height))))
    
    (super-new [paint-callback (λ (c dc) (my-paint-callback c dc))])))

(define canvas (new board-canvas% [parent game-panel]))

(define game-panel-side (new panel% [parent game-panel]))

(define (start-game difficulty)
  (set! current-board (new board% [difficulty difficulty]))
  (send choose-difficulty-panel reparent the-hidden-frame)
  (send game-panel reparent frame))

(define choose-difficulty-panel (new vertical-panel% [parent frame]))
(define vert-pane (new vertical-pane% [parent choose-difficulty-panel]))
(define button-pane (new horizontal-pane%
                         [parent choose-difficulty-panel]
                         [alignment '(center top)]))

(define msg (new message%
                 [parent vert-pane]
                 [label "Choose Difficulty:"]))

(define easy-button (new button%
                         [parent button-pane]
                         [label "Easy"]
                         [callback (λ (button event) (start-game 1))]))
(define medium-button(new button%
                          [parent button-pane]
                          [label "Medium"]
                          [callback (λ (button event) (start-game 2))]))
(define hard-button (new button%
                         [parent button-pane]
                         [label "Hard"]
                         [callback (λ (button event) (start-game 3))]))

(send frame fullscreen #f)
(send frame show #t)
