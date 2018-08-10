#lang racket/base

(require "tetris-implementation.rkt")

;; Tetris

;; we can represent the Tetris well and the collision
;; structure as a 2D vector (of vectors)
;; the top level vector is vertical
;; the vectors it holds represent the horizontal rows

(define COLS 10)
(define ROWS 24)

;; XXX Components
(define-component Shape shape)
(define-component Color color)
(define-component Position posn)
(define-component Held)
(define-component QueueX)
(define-component Active) 
(define-component Score counter) 
(define-component Timer counter) 

;;; XXX Archetypes

(define-archetype (ActiveTetromino [shape (random-shape)] [color (random-color)])
  (list Shape Color Position Active))

(define-archetype (Block [shape (unit-block)] [color (random-color)])
  (list Shape Color Position)) 

;;; XXX Events

(struct graphic (x y color))

(define-event key/e)
(define-event clock-tick/e)
(define-event collision-structure/e)
(define-event game-over/e) 
(define-event sound-effect/e) 
(define-event music/e)
(define-event graphic-event/e) 

;; XXX Systems

(define-system tetros-to-blocks   
  ;;#:archetype ActiveTetromino
  #:in [collision-structure/e 1]
  #:in [move-down/e 1]
  #:in [touched-bottom?/e 1]
  #:state [stated 5]
  #:pre pre (+ stated 3)
  #:enabled? (< pre 1)
  #:map mapfn (λ (e) (tetro-to-blocks e))
  #:reduce reduce (λ (x y) #f) (λ (x y) #t)
  #:post (λ (x) #t)) 

(define-system compute-collision-structure    
  ;;#:archetype Block
  #:in [tetros-to-blocks/e 1]
  #:in [touched-bottom?/e 1]
  #:map mapfn (λ (e) 
          (vector-set! 
           (vector-ref collision-structure/e 'e.Position.y) 
           'e.Position.x
           #t))
  #:out [collision-structure/e #t]) 

(define-system can-rotate-ccw?    
  ;;#:archetype ActiveTetromino
  #:in [collision-structure/e #t]
  #:in [key/e 'f]
  #:map mapfn (λ (e) (valid-ccw? e collision-structure/e)))

(define-system can-rotate-cw?    
  ;;#:archetype ActiveTetromino
  #:in [collision-structure/e #t]
  #:in [key/e 'x]
  #:map mapfn (λ (e) (valid-cw? e collision-structure/e)))

(define-system can-move-down?    
  ;;#:archetype ActiveTetromino
  #:in [collision-structure/e #t]
  #:in [key/e 'm]
  #:map mapfn (λ (e) (vacant-down? e collision-structure/e)))

(define-system can-move-right?    
  ;;#:archetype ActiveTetromino
  #:in [collision-structure/e #t]
  #:in [key/e 'right]
  #:map mapfn (λ (e) (vacant-right? e collision-structure/e)))

(define-system can-move-left?    
  ;;#:archetype ActiveTetromino
  #:in [collision-structure/e #t]
  #:in [key/e 'left]
  #:map mapfn (λ (e) (vacant-left? e collision-structure/e)))

(define-system new-tetro
  ;;#:archetype ActiveTetromino
  ;;#:in (events touched-bottom?)
  #:post (λ (x) #t) ;; create new entity
  )

(define-system touched-bottom?    
  ;;#:archetype ActiveTetromino
  #:in [collision-structure/e #t]
  #:in [clock-tick/e 'changed]
  #:map mapfn (λ (e)
          (vector-ref 
           (vector-ref collision-structure/e (sub1 'e.Position.y)) 
           'e.Position.x)))

(define-system check-block-overflow   
  ;;#:archetype ActiveTetromino
  #:in [collision-structure/e #t]
  #:map mapfn (λ (e) (< 'e.Position.y 0)))

(define-system clear-full-rows
  #:in [collision-structure/e #t]
  #:in [touched-bottom?/e #t]
  #:map mapfn (λ (e) 
          (when #t (set! e (make-vector COLS #f)))))

(define-system increment-timer
  ;;#:archetype Timer    
  #:in [clock-tick/e 'changed]
  #:map mapfn (λ (e) (set! e.Timer.val (add1 e.Timer.val))))

(define-system increment-score
  ;;#:archetype Score
  #:in [collision-structure/e #t]
  #:in [increment-timer/e #t]
  #:map mapfn (λ (e) (set! e.Score.val (add1 e.Score.val))))

(define-system hard-drop    
  ;;#:archetype ActiveTetromino
  #:in [collision-structure/e #t]
  #:in [key/e 's]
  #:map mapfn (λ (e) (set! e.Position.y (lowest-y e collision-structure/e))))

(define-system soft-drop    
  ;;#:archetype ActiveTetromino
  #:in [collision-structure/e #t]
  #:in [key/e 'x]
  #:map mapfn (λ (e) (set! e.Position.y (- e.Position.y 3))))

(define-system rotate-ccw    
  ;;#:archetype ActiveTetromino
  #:in [collision-structure/e #t]
  #:in [can-rotate-ccw?/e #t]
  #:map mapfn (λ (e) (rotate90 (rotate90 (rotate90 e)))))

(define-system rotate-cw    
  ;;#:archetype ActiveTetromino
  #:in [collision-structure/e #t]
  #:in [can-rotate-cw?/e #t]
  #:map mapfn (λ (e) (rotate90 e)))

(define-system move-down    
  ;;#:archetype ActiveTetromino
  #:in [clock-tick/e 'change]
  #:in [can-move-down?/e #t]
  #:map mapfn (λ (e)
          (set! e.Position.y (sub1 e.Position.y))))

(define-system move-right    
  ;;#:archetype ActiveTetromino
  #:in [collision-structure/e #t]
  #:in [can-move-right?/e #t]
  #:map mapfn (λ (e)
          (set! e.Position.x (add1 e.Position.x))))

(define-system move-left    
  ;;#:archetype ActiveTetromino
  #:in [collision-structure/e #t]
  #:in [can-move-left?/e #t]
  #:map mapfn (λ (e)
          (set! e.Position.x (sub1 e.Position.x))))

; XXX Worlds

(define-world tetris start!)