#lang racket/base
(require recess/run-lux-mode-lambda "helpers.rkt")

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

(define-system move-player
  #:in [key key/e]
  #:query player (lookup Player)
  #:map pos (get player 'Position) (move-player! player (and key (key-event-code key))))

(define-system render-player
  #:in [on-move move-player]
  #:query player (lookup Player)
  #:map pos (get player 'Position)
  #:out [image/e (draw-entities pos 'ellipse-0)])

(define-system render-enemies
  #:query en (lookup Enemy)
  #:map pos (get en 'Position)
  #:out [image/e (draw-entities pos 'ellipse-2)])

(define-system enemy-impact
  #:query en (lookup Enemy Alive)
  #:map _
  (when
      (close-enough? .5 (get en 'Position) (get-entity-posns (lookup Bullet)))
    (- (+ en Dead) Alive)))

(define-system enemy-death
  #:in [on-impact enemy-impact]
  #:query en (lookup Enemy Dead)
  #:map _ (remove-entity! en))

(define-system bullet-motion
  #:query bullet (lookup Bullet)
  #:map pos (set! bullet (move-bullet (get bullet 'Position)) 'Position))

(define-system render-bullets
  #:in [on-motion bullet-motion]
  #:query bullet (lookup Bullet)
  #:map pos (get bullet 'Position)
  #:out [image/e (draw-entities pos 'ellipse-1)])

(define-system shoot
  #:in [key key/e]
  #:post (h-align-shot (list Bullet Position) key (car (lookup Player))))

(begin-recess
  #:systems
  render-player render-bullets 
  move-player bullet-motion shoot
  render-enemies enemy-impact enemy-death
  #:initialize
  (add-entity! (list Player Position))
  (let ([enemies (list (make-posn 160 100) (make-posn 240 100))])
    (for-each
     (λ (pos) (add-entity! (list Enemy Alive (create-component 'Position pos)))) 
    enemies))
  #:stop #f
  #:run run/lux-mode-lambda)
