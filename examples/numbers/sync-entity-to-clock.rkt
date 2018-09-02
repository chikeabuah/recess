#lang racket/base

(require recess/run-big-bang)
     
(define-component Count 0) 

;; the idea behind this system is to synchronize
;; a numeric entity with a time clock
(define-system sync-to-clock
  #:in [seconds clock/e]
  #:pre x (sleep 1) (displayln seconds)
  ;; this system is enabled as long as the clock's value is less
  ;; than 15 seconds
  #:enabled? (< seconds 15)
  ;; query for all entities that have the component/archetype: Count
  ;; we know there will only be one
  #:query e (lookup Count)
  ;; every iteration increment e by 1 and print it 
  #:map _ (set! e (+ (get e 'Count) 1)) (displayln (get e 'Count)))

(module+ main
 (begin-recess
  #:systems sync-to-clock
  #:initialize (add-entity! (list Count))
  #:stop (because #:systems sync-to-clock)
  #:run run/big-bang))
  