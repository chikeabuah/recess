#lang racket/base

(require recess/run-lux-mode-lambda)

(provide (all-defined-out))

;; helpers

(define (draw-entities posns sprite-sym)
  (map
   (λ (position) (cons sprite-sym position))
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

(define (get-entity-posns ents)
  (map
   (λ (ent) (get ent 'Position))
   ents))

(define (distance a b)
  (sqrt
   (+
    (expt (- (posn-x a) (posn-x b)) 2)
    (expt (- (posn-y a) (posn-y b)) 2))))

(define (close-enough? thresh pos posns)
  (define poslst (make-list (length posns) pos))
  (define distances (map distance poslst posns))
  (displayln distances)
  (ormap (λ (d) (< d thresh)) distances))
