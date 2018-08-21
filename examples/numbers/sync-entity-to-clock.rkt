#lang racket/base

(require recess)

(struct counter (x)
  #:methods gen:component-prototype-generic
  [(define (init-component component-prototype-generic)
     (counter 0))])
     
(define-component Count counter) 

;; the idea behind this system is to synchronize
;; a numeric entity with a time clock
(define-system sync-to-clock
  ;; every second
  #:in [seconds clock/e]
  ;; query for all entities that have the component/archetype: Count
  ;; we know there will only be one so bind it to e
  #:query e (Count)
  ;; this system is enabled as long as the clock's value is less
  ;; than 15 seconds 
  #:enabled? (< seconds 15)
  ;; every iteration increment e by 1 and print it
  #:post (add1! e) (display e))

(module+ main
 (begin-recess
  #:systems sync-to-clock
  #:initialize (add-entity! (Count)) (set-event! clock/e 0)
  #:stop-when sync-to-clock))
  