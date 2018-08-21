#lang racket/base

(require recess)

(struct counter (x)
  #:methods gen:component-prototype-generic
  [(define (init-component component-prototype-generic)
     (counter 0))])
     
(define-component Count counter) 

;; the idea behind this system is to simulate the factorial
;; operation using numeric entities
(define-system factorial
  ;; every second
  #:in [seconds clock/e]
  ;; query for all entities that have the component/archetype: Count
  ;; bind the entities to the list ents
  ;; note: this is guaranteed to be an ordered list
  ;; towards the end of the list are the most recent entities and thus
  ;; the largest numbers
  #:query ents (Count)
  ;; this system is enabled as long as the clock's value is less
  ;; than 12 seconds 
  #:enabled? (< seconds 12)
  ;; compute factorial
  #:reduce fac 1 (λ (a b) (* a b))
  ;; every iteration create a new entity equal to the last one but incremented by 1
  ;; and print the current factorial
  #:post (display fac) (create-entity! (+ (last entities) 1)))

(module+ main
 (begin-recess
  #:systems factorial
  #:initialize (add-entity! (Count)) (set-event! clock/e 0)
  #:stop-when factorial))
  