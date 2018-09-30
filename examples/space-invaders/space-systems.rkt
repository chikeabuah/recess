#lang racket/base
(require recess/run-lux-mode-lambda)

(define-component Position (make-posn 200 350))
(define-component Friendly)
(define-component Enemy)
(define-component Neutral)
(define-component Value 0)
(define-component Count 0)
(define-component Bullet)
(define-component Thing)
(define-component Player)
(define-component Health 0)
(define-component Alive)
(define-component Dead)
(define-component MoveRate 3)
(define-component FireDelay 3)

;; assuming there can be multiple players at once
(define (draw-players posns)
  (map
   (λ (position) (cons 'ellipse position))
   posns))

(define-system render-players
  #:in [seconds clock/e]
  #:query player (lookup Player)
  #:map pos (get player 'Position)
  #:out [image/e (draw-players pos)])


(define-system move-player-1
  #:in [seconds clock/e]
  #:in [key key/e]
  #:query player (lookup Player)
  #:map pos (get player 'Position) (display (and key (key-event-code key))))

(begin-recess
  #:systems render-players move-player-1
  #:initialize (add-entity! (list Player Position))
  #:stop #f
  #:run run/lux-mode-lambda)
