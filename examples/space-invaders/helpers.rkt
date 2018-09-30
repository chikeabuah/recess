#lang racket/base

(require recess/run-lux-mode-lambda)

(provide (all-defined-out))

;; helpers

;; assuming there can be multiple players
(define (draw-players posns)
  (map
   (λ (position) (cons 'ellipse position))
   posns))

(define (move-player! player key)
  (define k
    (if (and key (not (pair? key)))
        key
        #f))
  (define offset 
    (cond 
      [(eq? k 'left) -20]
      [(eq? k 'right) 20]
      [else 0]))
  (define player-posn (get player 'Position))
  (define new-posn (make-posn (+ (posn-x player-posn) offset) (posn-y player-posn)))
  (set! player new-posn 'Position))

(define (move-bullet pos)
  (make-posn (posn-x pos) (- (posn-y pos) 2)))

(define (h-align-shot bullet key player)
  (define player-posn (get player 'Position))
  (when (and key (eq? (key-event-code key) #\s))
    (set! (add-entity! bullet) player-posn 'Position)))