#lang racket/base

(require recess/run-lux-mode-lambda "helpers.rkt")

(define PARTICLES 200)
(define W 3000)
(define H 2000)
(define-component Particle)
(define-component Position (make-posn 1000 1000))
(define-component Shape)
(define-component Color)
(define-component Mobile)
(define-component Size 5)
(define-component Effect)
(define-component BoundingX)
(define-component BoundingY)
(define-component Mass)
(define-component SpeedX 5)
(define-component SpeedY 5)
(define-component GameID)

(define-system particle-collision
  #:query particle (lookup Particle Position Mobile)
  #:map _ (collide! particle (lookup Particle Position Mobile)))

(define-system move-particles
  #:in [on-collide particle-collision]
  #:query particle (lookup Particle Position Mobile)
  #:map pos (move-bounded-particle! particle W H))

(define-system render-particles
  #:in [on-move move-particles]
  #:query particle (lookup Particle Position)
  #:map pos (get particle 'Position)
  #:out [image/e (draw-entities pos 'ellipse-0)])

(begin-recess
  #:systems
  render-particles  
  move-particles
  particle-collision
  #:initialize
  ;; add particles
  (for ([idx (in-range PARTICLES)])
      (add-entity!
        (list Particle Mobile Size
              (let* ([r (random 2)]
                     [s (if (eq? r 0) 1 -1)])
                (copy-component 'SpeedX (* s (add1 (random 15)))))
              (let* ([r (random 2)]
                     [s (if (eq? r 0) 1 -1)])
                (copy-component 'SpeedY (* s (add1 (random 15)))))
              (copy-component 'GameID (gensym))
              (copy-component 'Mass (add1 (random 20)))
              (copy-component 'Position (make-posn (random W) (random H))))))
  #:stop #f
  #:run run/lux-mode-lambda)
