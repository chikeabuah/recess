#lang racket/base

(require recess/run-lux-mode-lambda "helpers.rkt")

(define-component Position (make-posn 340 600))
;; in space invaders enemies initially cycle by some offset around a fixed axis
;; when the player clears out enough columns the remaining enemies
;; begin to span the entire screen very quickly (axis is now centre of screen)
(define-component Axis)
(define-component MaxOffset 40)
(define-component CurrentOffset 0)
;; enemy direction flag
(define-component Polarity #t)
(define-component FirstWall #f)
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

;; design thoughts
;; if we can build this game without needing state in any of the systems it would be
;; more pure/functional in a sense

(define-system move-player
  #:in [key key/e]
  #:query player (lookup Player)
  #:map pos (get player 'Position) (move-player! player (and key (key-event-code key))))

(define-system render-player
  #:in [on-move move-player]
  #:query player (lookup Player)
  #:map pos (get player 'Position)
  #:out [image/e (draw-entities pos 'ellipse-0)])

(define-system enemy-horizontal-motion
  #:query en (lookup Enemy Alive)
  ;; increment offset, move in some direction
  #:map _ (move-enemy-h en))

(define-system render-enemies
  #:in [on-h-move enemy-horizontal-motion]
  #:query en (lookup Enemy)
  #:map pos (get en 'Position)
  #:out [image/e (draw-entities pos 'ellipse-2)])

;; an approach to programming the enemies motion
;; two alternating systems - horizontal and vertical

;; the vertical system should only be enabled when the horizontal
;; system has moved up to the offset amount
;; so the vertical system can depend on the horizontal and listen for a signal

;; we're either moving left or right so binary state

(define-system enemy-impact
  #:in [on-move enemy-horizontal-motion]
  #:query en (lookup Enemy Alive)
  #:map _
  (when
      (close-enough? 10 (get en 'Position) (get-entity-posns (lookup Bullet)))
    (- (+ en Dead) Alive)))

(define-system enemy-death
  #:in [on-impact enemy-impact]
  #:query en (lookup Enemy Dead)
  #:map _ (remove-entity! en))

(define-system bullet-motion
  #:query bullet (lookup Bullet)
  #:map pos (~>! bullet (move-bullet (get bullet 'Position)) 'Position))

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
  render-enemies enemy-impact enemy-death enemy-horizontal-motion
  #:initialize
  ;; add player(s)
  (add-entity! (list Player Position))
  ;; add enemies
  (let ([enemies (flatten enemy-matrix)])
    (for-each
     (λ (pos)
       (add-entity!
        (list Enemy Alive CurrentOffset MaxOffset Polarity FirstWall
              (create-component 'Position pos)
              (create-component 'Axis pos)))) 
     enemies))
  #:stop #f
  #:run run/lux-mode-lambda)
