#lang racket/base

(require recess)
     
(define-component Count 0) 

;; the idea behind this system is to create a pattern using a numeric entity
;; we add increasing even numbers to get the next number
;; for example: 
;; 0,+2 2,+4 6,+6 12,+8 20,+10 30,+12 42, …
;; we can model the system's state as the increasing even number
;; and the entity is the base number
(define-system sync-to-clock
  #:in [seconds clock/e]
  ;; there is a binding called x in our state
  #:state [x 0]
  #:pre y (sleep 3) (displayln x) (+ x 2)
  ;; this system is enabled as long as the clock's value is less
  ;; than 15 seconds 
  #:enabled? (< seconds 15)
  ;; query for all entities that have the component/archetype: Count
  ;; we know there will only be one so bind it to e
  #:query e (lookup Count)
  #:map _ (set! e (+ (get e 'Count) y)) (display (get e 'Count)))

(module+ main
 (begin-recess
  #:systems sync-to-clock
  #:initialize (add-entity! (list Count)) (set-event! clock/e 0)
  #:stop-when sync-to-clock))
  