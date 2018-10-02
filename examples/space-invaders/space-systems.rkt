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
(define-component Unfriendly)
(define-component Enemy)
(define-component Neutral)
(define-component Value 0)
(define-component Score 0)
(define-component Bullet)
(define-component Thing)
(define-component Player)
(define-component Health 0)
(define-component Alive)
(define-component Dead)
(define-component MoveRate 3)
(define-component FireDelay 200)

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

(define-system enemies-shoot
  #:query en (lookup Enemy)
  #:map _ (shoot-from-enemy en (list Bullet Unfriendly Position)))

(define-system enemy-bullet-motion
  #:query bullet (lookup Bullet Unfriendly)
  #:map pos (~>! bullet (move-unfriendly-bullet (get bullet 'Position)) 'Position))

(define-system enemy-impact
  #:in [on-move enemy-horizontal-motion]
  #:query en (lookup Enemy Alive)
  #:map _
  (when
      (close-enough? 10 (get en 'Position) (get-entity-posns (lookup Bullet Friendly)))
    (let* ([e (car (lookup Score))]
          [score (get e 'Score)])
      (~>! e (+ score 1) 'Score)
      (displayln (get e 'Score)))
    (- (+ en Dead) Alive)))

(define-system enemy-death
  #:in [on-impact enemy-impact]
  #:query en (lookup Enemy Dead)
  #:map _ (remove-entity! en))

(define-system friendly-bullet-motion
  #:query bullet (lookup Bullet Friendly)
  #:map pos (~>! bullet (move-friendly-bullet (get bullet 'Position)) 'Position))

(define-system render-friendly-bullets
  #:in [on-motion friendly-bullet-motion]
  #:query bullet (lookup Bullet Friendly)
  #:map pos (get bullet 'Position)
  #:out [image/e (draw-entities pos 'ellipse-1)])

(define-system render-unfriendly-bullets
  #:in [on-motion enemy-bullet-motion]
  #:query bullet (lookup Bullet Unfriendly)
  #:map pos (get bullet 'Position)
  #:out [image/e (draw-entities pos 'ellipse-3)])

(define-system shoot
  #:in [key key/e]
  #:post (h-align-shot (list Bullet Friendly Position) key (car (lookup Player))))

(define-system render-score
  #:query s (lookup Score)
  #:map s s
  #:out [image/e (draw-number (car s))])

(begin-recess
  #:systems
  render-player render-friendly-bullets 
  move-player friendly-bullet-motion shoot
  render-enemies enemy-impact enemy-death
  enemy-horizontal-motion enemy-bullet-motion
  enemies-shoot render-unfriendly-bullets
  render-score
  #:initialize
  ;;score
  (add-entity! (list Score))
  ;; add player(s)
  (add-entity! (list Player Position))
  ;; add enemies
  (let ([enemies (flatten enemy-matrix)])
    (for-each
     (λ (pos)
       (add-entity!
        (list Enemy Alive CurrentOffset
              MaxOffset Polarity FirstWall
              FireDelay
              (create-component 'Position pos)
              (create-component 'Axis pos)))) 
     enemies))
  #:stop #f
  #:run run/lux-mode-lambda)
