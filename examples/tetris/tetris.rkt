#lang s-exp "recess.rkt"

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
(define-component Time ([val 0])) 
;
;;; XXX Archetypes
;
;(define-archetype (ActiveTetro [shape (random-shape)] [color (random-color)])
;  (Shape shape) (Color color)
;  (Position) (Active))
;
;(define-archetype (Block [shape (unit-block)] [color (random-color)])
;  (Shape shape) (Color color)
;  (Position)) 
;
;;; XXX Events
;
;(define-event key-event key-event?)
;(define-event clock-tick time-event?)
;(define-event collision-structure vector? (make-vector ROWS (make-vector COLS #f)))
;;; Boolean events will most likely have an equivalent system responsible
;;; for determining and relaying these events. This could be an opportunity
;;; for a useful idiom i.e. if a system is subscribed to an event, it also
;;; depends on the equivalent relaying system implicitly.
;(define-event touched-bottom? boolean?) 
;(define-event can-rotate-cw? boolean?) 
;(define-event can-rotate-ccw? boolean?) 
;(define-event can-move-left? boolean?) 
;(define-event can-move-right? boolean?) 
;(define-event can-move-down? boolean?) 
;(define-event game-over) 
;(define-event sound-effect) 
;(define-event music)
;(struct graphic (x y color) #:mutable)
;(define-event graphic graphic?) 
;
;
;;; XXX Systems
;
;(define-system handle-input    
;  #:out key-event)    
;
;(define-system compute-collision-structure    
;  #:archetype Block
;  #:on touched-bottom? collision-structure
;  #:out collision-structure
;  #:depends tetro-to-blocks
;  #:map (lambda (e) 
;    (vector-set! 
;      (vector-ref collision-structure e.Position.y) 
;      e.Position.x
;      #t)))        
;
;(define-system touched-bottom?    
;  #:archetype ActiveTetromino
;  #:on clock-tick collision-structure
;  #:out touched-bottom?
;  #:map (lambda (e)
;    (vector-ref 
;      (vector-ref collision-structure (sub1 e.Position.y)) 
;      e.Position.x)))
;  
;
;(define-system check-block-overflow   
;  #:archetype ActiveTetromino
;  #:on collision-structure
;  #:out touched-bottom?
;  #:depends compute-collision-structure
;  #:map (lambda (e) (< e.Position.y 0)))
;
;
;(define-system tetros-to-blocks   
;  #:archetype ActiveTetromino
;  #:on touched-bottom?
;  #:depends move-down
;  #:map (lambda (e) (tetro-to-blocks e)))
;
;
;(define-system clear-full-rows
;  #:on collision-structure touched-bottom?
;  #:depends compute-collision-structure
;  #:map collision-structure (lambda (e) 
;    (if (apply and e) (set! e (make-vector COLS #f)))))
;
;
;(define-system increment-timer
;  #:archetype Timer    
;  #:on clock-tick
;  #:map (lambda (e) (set! e.Timer.val (add1 e.Timer.val))))
;
;
;(define-system increment-score
;  #:archetype Score  
;  #:on touched-bottom?
;  #:map (lambda (e) (set! e.Score.val (add1 e.Score.val))))
;
;
;(define-system hard-drop    
;  #:archetype ActiveTetromino
;  #:on (eq? key-event 'down) collision-structure
;  #:depends compute-collision-structure move-down
;  #:map (lambda (e) (set! e.Position.y (lowest-y e collision-structure))))
;
;
;(define-system soft-drop    
;  #:archetype ActiveTetromino
;  #:on (eq? key-event 'd) collision-structure
;  #:depends compute-collision-structure move-down
;  #:map (lambda (e) (set! e.Position.y (- e.Position.y 3))))
;
;
;(define-system rotate-ccw    
;  #:archetype ActiveTetromino
;  #:on (eq? can-rotate-ccw? #t) collision-structure
;  #:depends compute-collision-structure
;  #:map (lambda (e) (rotate90 (rotate90 (rotate90 e)))))
;
;
;(define-system rotate-cw    
;  #:archetype ActiveTetromino
;  #:on (eq? can-rotate-cw? #t) collision-structure
;  #:depends compute-collision-structure
;  #:map (lambda (e) (rotate90 e)))
;
;(define-system move-down    
;  #:archetype ActiveTetromino
;  #:on clock-tick (eq? can-move-down? #t)
;  #:map (lambda (e)
;    (set! e.Position.y (sub1 e.Position.y))))
;
;(define-system move-right    
;  #:archetype ActiveTetromino
;  #:on (eq? can-move-right? #t)
;  #:map (lambda (e)
;    (set! e.Position.x (add1 e.Position.x))))
;
;(define-system move-left    
;  #:archetype ActiveTetromino
;  #:on (eq? can-move-left? #t)
;  #:map (lambda (e)
;    (set! e.Position.x (sub1 e.Position.x))))
;
;(define-system can-rotate-ccw?    
;  #:archetype ActiveTetromino
;  #:on collision-structure
;  #:out can-rotate-ccw?
;  #:depends compute-collision-structure
;  #:map (lambda (e) (valid-ccw? e collision-structure)))
;
;(define-system can-rotate-cw?    
;  #:archetype ActiveTetromino
;  #:on collision-structure
;  #:out can-rotate-cw?
;  #:depends compute-collision-structure
;  #:map (lambda (e) (valid-cw? e collision-structure)))
;
;(define-system can-move-down?    
;  #:archetype ActiveTetromino
;  #:on collision-structure
;  #:out can-move-down?
;  #:depends compute-collision-structure
;  #:map (lambda (e) (vacant-down? e collision-structure)))
;
;
;(define-system can-move-right?    
;  #:archetype ActiveTetromino
;  #:on collision-structure
;  #:out can-move-right?
;  #:depends compute-collision-structure
;  #:map (lambda (e) (vacant-right? e collision-structure)))
;
;
;(define-system can-move-left?    
;  #:archetype ActiveTetromino
;  #:on collision-structure
;  #:out can-move-left?
;  #:depends compute-collision-structure
;  #:map (lambda (e) (vacant-left? e collision-structure)))


;; XXX Worlds
