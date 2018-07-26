#lang racket/base
(require
  "recess-graph.rkt"
  "tetris-helpers.rkt")

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

(define-event key/e key-event)
(define-event clock-tick/e time-event)
(define-event collision-structure/e vector)
(define-event game-over/e) 
(define-event sound-effect/e) 
(define-event music/e)
(define-event graphic-event/e graphic) 

;; XXX Systems

(define-system tetros-to-blocks   
  #:archetype ActiveTetromino
  #:on (events collision-structure/e move-down touched-bottom?)
  #:init 5
  #:enabled #t
  #:zero state
  #:reduce (λ (x y) #t)
  #:post (λ (x) #t)
  #:pre (+ state 3)
  #:map (λ (e) (tetro-to-blocks e))) 

(define-system compute-collision-structure    
  #:archetype Block
  #:on (events tetros-to-blocks touched-bottom?)
  #:out (events collision-structure/e)
  #:map (λ (e) 
          (vector-set! 
           (vector-ref collision-structure/e 'e.Position.y) 
           'e.Position.x
           #t))) 

(define-system can-rotate-ccw?    
  #:archetype ActiveTetromino
  #:on (events collision-structure/e key/e)
  #:map (λ (e) (valid-ccw? e collision-structure/e)))

(define-system can-rotate-cw?    
  #:archetype ActiveTetromino
  #:on (events collision-structure/e key/e)
  #:map (λ (e) (valid-cw? e collision-structure/e)))

(define-system can-move-down?    
  #:archetype ActiveTetromino
  #:on (events collision-structure/e key/e)
  #:map (λ (e) (vacant-down? e collision-structure/e)))

(define-system can-move-right?    
  #:archetype ActiveTetromino
  #:on (events collision-structure/e key/e)
  #:map (λ (e) (vacant-right? e collision-structure/e)))

(define-system can-move-left?    
  #:archetype ActiveTetromino
  #:on (events collision-structure/e key/e)
  #:map (λ (e) (vacant-left? e collision-structure/e)))

(define-system new-tetro
  #:archetype ActiveTetromino
  #:on (events touched-bottom?)
  #:post (λ (x) #t) ;; create new entity
  )

(define-system touched-bottom?    
  #:archetype ActiveTetromino
  #:on (events clock-tick/e collision-structure/e)
  #:map (λ (e)
          (vector-ref 
           (vector-ref collision-structure/e (sub1 'e.Position.y)) 
           'e.Position.x)))

(define-system check-block-overflow   
  #:archetype ActiveTetromino
  #:on (events collision-structure/e)
  #:map (λ (e) (< 'e.Position.y 0)))

(define-system clear-full-rows
  #:on (events collision-structure/e touched-bottom?)
  #:map (λ (e) 
          (when #t (set! e (make-vector COLS #f)))))

(define-system increment-timer
  #:archetype Timer    
  #:on (events clock-tick/e)
  #:map (λ (e) (set! e.Timer.val (add1 e.Timer.val))))

(define-system increment-score
  #:archetype Score
  #:on (events collision-structure/e increment-timer)
  #:map (λ (e) (set! e.Score.val (add1 e.Score.val))))

(define-system hard-drop    
  #:archetype ActiveTetromino
  #:on (events key/e collision-structure/e move-down)
  #:map (λ (e) (set! e.Position.y (lowest-y e collision-structure/e))))

(define-system soft-drop    
  #:archetype ActiveTetromino
  #:on (events key/e collision-structure/e move-down)
  #:map (λ (e) (set! e.Position.y (- e.Position.y 3))))

(define-system rotate-ccw    
  #:archetype ActiveTetromino
  #:on (events collision-structure/e can-rotate-ccw?)
  #:map (λ (e) (rotate90 (rotate90 (rotate90 e)))))

(define-system rotate-cw    
  #:archetype ActiveTetromino
  #:on (events collision-structure/e can-rotate-cw?)
  #:map (λ (e) (rotate90 e)))

(define-system move-down    
  #:archetype ActiveTetromino
  #:on (events clock-tick/e can-move-down?)
  #:map (λ (e)
          (set! e.Position.y (sub1 e.Position.y))))

(define-system move-right    
  #:archetype ActiveTetromino
  #:on (events collision-structure/e can-move-right?)
  #:map (λ (e)
          (set! e.Position.x (add1 e.Position.x))))

(define-system move-left    
  #:archetype ActiveTetromino
  #:on (events collision-structure/e can-move-left?)
  #:map (λ (e)
          (set! e.Position.x (sub1 e.Position.x))))

; XXX Worlds