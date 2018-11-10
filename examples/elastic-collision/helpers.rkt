#lang racket/base

(require recess/run-lux-mode-lambda)

(provide (all-defined-out))

;; helpers

(define (draw-entities posns sprite-sym)
  (map
   (λ (position) (cons sprite-sym position))
   posns))

(define (move-bounded-particle! particle W H)
  (define px (posn-x (get particle 'Position)))
  (define py (posn-y (get particle 'Position)))
  (define vx (get particle 'SpeedX))
  (define vy (get particle 'SpeedY))
  (define r (get particle 'Size))
  (when (<= (- px r) 0)
    (set! px r))
  (when (>= (+ px r) W)
    (set! px (- W r)))  
  (when (or (and (<= (- px r) 0) (< vx 0)) (and (>= (+ px r) W) (> vx 0)))
    (set! vx (* vx -1)))
  (when (<= (- py r) 0)
    (set! py r))
  (when (>= (+ py r) H)
    (set! px (- H r)))  
  (when (or (and (<= (- py r)  0) (< vy  0)) (and (>= (+ py r) W) (> vy 0)))
    (set! vy (* vy -1)))
  (define new-posn (make-posn (+ px vx) (+ py vy)))
  (~~>! particle (make-immutable-hasheq
                  (list
                   (cons 'SpeedX vx)
                   (cons 'SpeedY vy)
                   (cons 'Position new-posn)))))
