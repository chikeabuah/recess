#lang racket/base

(require recess)

(struct counter (x)
  #:methods gen:component-prototype-generic
  [(define (init-component component-prototype-generic)
     (counter 0))])
     
(define-component Count counter) 
(define-event clock/e)

;; the idea behind this system is to create a pattern using a numeric entity
;; we add increasing even numbers to get the next number
;; for example: 
;; 0,+2 2,+4 6,+6 12,+8 20,+10 30,+12 42, …
;; we can model the system's state as the increasing even number
;; and the entity is the base number
(define-system sync-to-clock
  ;; every second
  #:in [seconds clock/e]
  ;; there is a binding called x in our state
  ;; initially it has the value of 5
  #:state [x 2]
  ;; query for all entities that have the component/archetype: Count
  ;; we know there will only be one so bind it to e
  #:query e (Count)
  ;; this system is enabled as long as the clock's value is less
  ;; than 15 seconds 
  #:enabled? (< seconds 20)
  ;; every iteration increment e by 1 and print it
  #:post (set! e (+ e x)) (display e) (set! x (+ x 2)))

(module+ main
 (begin-recess
  #:systems sync-to-clock
  #:initialize (add-entity! (Count)) (set-event! clock/e 0)
  #:stop-when sync-to-clock))
  
  