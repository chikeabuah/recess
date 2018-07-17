#lang s-exp "recess-graph.rkt"

;; Tetris

;; we can represent the Tetris well and the collision
;; structure as a 2D vector (of vectors)
;; the top level vector is vertical
;; the vectors it holds represent the horizontal rows


(define COLS 10)
(define ROWS 24)

;; XXX Components
(define-component Shape ([which (define-enum Block I J L O S T Z)]))
(define-component Color ([rgb (random-color)]))
(define-component Position ([x 0] [y 0]))
(define-component Held)
(define-component QueueX)
(define-component Active) 
(define-component Score ([val 0])) 
(define-component Timer ([val 0])) 

;;; XXX Archetypes

(define-archetype (ActiveTetromino [shape ('random-shape)] [color ('random-color)])
  '(Shape Color Position Active))

(define-archetype (Block [shape ('unit-block)] [color ('random-color)])
  '(Shape Color Position)) 

;;; XXX Events

(struct graphic (x y color) #:mutable)

(define-event key-event key-event?)
(define-event clock-tick time-event?)
(define-event collision-structure vector? (make-vector ROWS (make-vector COLS #f)))
(define-event game-over) 
(define-event sound-effect) 
(define-event music)
(define-event graphic-event graphic?) 

;; XXX Systems

(define-system key-event)    

(define-system compute-collision-structure    
  #:archetype Block
  #:on '(collision-structure)
  #:out '(collision-structure)
  #:depends '(tetros-to-blocks touched-bottom?)
  #:map (lambda (e) 
    (vector-set! 
      (vector-ref collision-structure 'e.Position.y) 
      'e.Position.x
      #t)))        

(define-system touched-bottom?    
  #:archetype ActiveTetromino
  #:on '(clock-tick collision-structure)
  #:out '(touched-bottom?)
  #:map (lambda (e)
    (vector-ref 
      (vector-ref collision-structure (sub1 'e.Position.y)) 
      'e.Position.x)))

(define-system check-block-overflow   
  #:archetype ActiveTetromino
  #:on '(collision-structure)
  #:out '(touched-bottom?)
  #:depends '(compute-collision-structure)
  #:map (lambda (e) (< 'e.Position.y 0)))

(define-system tetros-to-blocks   
  #:archetype ActiveTetromino
  #:depends '(move-down touched-bottom?)
  #:map (lambda (e) ('tetro-to-blocks e)))

(define-system clear-full-rows
  #:on '(collision-structure)
  #:depends '(compute-collision-structure touched-bottom?)
  #:map (lambda (e) 
    (when #t (set! e (make-vector COLS #f)))))

(define-system increment-timer
  #:archetype Timer    
  #:on '(clock-tick)
  #:map (lambda (e) (set! e.Timer.val (add1 e.Timer.val))))

(define-system increment-score
  #:archetype Score  
  #:depends '(touched-timer)
  #:map (lambda (e) (set! e.Score.val (add1 e.Score.val))))

(define-system hard-drop    
  #:archetype ActiveTetromino
  #:on '((eq? key-event 'down) collision-structure)
  #:depends '(compute-collision-structure move-down)
  #:map (lambda (e) (set! e.Position.y (lowest-y e collision-structure))))

(define-system soft-drop    
  #:archetype ActiveTetromino
  #:on '((eq? key-event 'd) collision-structure)
  #:depends '(compute-collision-structure move-down)
  #:map (lambda (e) (set! e.Position.y (- e.Position.y 3))))

(define-system rotate-ccw    
  #:archetype ActiveTetromino
  #:on '(collision-structure)
  #:depends '(compute-collision-structure can-rotate-ccw?)
  #:map (lambda (e) (rotate90 (rotate90 (rotate90 e)))))

(define-system rotate-cw    
  #:archetype ActiveTetromino
  #:on '(collision-structure)
  #:depends '(compute-collision-structure can-rotate-cw?)
  #:map (lambda (e) (rotate90 e)))

(define-system move-down    
  #:archetype ActiveTetromino
  #:on '(clock-tick)
  #:depends '(can-move-down)
  #:map (lambda (e)
    (set! e.Position.y (sub1 e.Position.y))))

(define-system move-right    
  #:archetype ActiveTetromino
  #:depends '(can-move-right?)
  #:map (lambda (e)
    (set! e.Position.x (add1 e.Position.x))))

(define-system move-left    
  #:archetype ActiveTetromino
  #:depends '(can-move-left?)
  #:map (lambda (e)
    (set! e.Position.x (sub1 e.Position.x))))

(define-system can-rotate-ccw?    
  #:archetype ActiveTetromino
  #:on '(collision-structure)
  #:depends '(compute-collision-structure handle-input)
  #:map (lambda (e) (valid-ccw? e collision-structure)))

(define-system can-rotate-cw?    
  #:archetype ActiveTetromino
  #:on '(collision-structure)
  #:depends '(compute-collision-structure handle-input)
  #:map (lambda (e) (valid-cw? e collision-structure)))

(define-system can-move-down?    
  #:archetype ActiveTetromino
  #:on '(collision-structure)
  #:depends '(compute-collision-structure handle-input)
  #:map (lambda (e) (vacant-down? e collision-structure)))

(define-system can-move-right?    
  #:archetype ActiveTetromino
  #:on '(collision-structure)
  #:depends '(compute-collision-structure handle-input)
  #:map (lambda (e) (vacant-right? e collision-structure)))

(define-system can-move-left?    
  #:archetype ActiveTetromino
  #:on '(collision-structure)
  #:depends '(compute-collision-structure handle-input)
  #:map (lambda (e) (vacant-left? e collision-structure)))

; XXX Worlds
