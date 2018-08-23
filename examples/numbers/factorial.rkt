#lang racket/base

(require recess)
     
(define-component Count 2) 

;; the idea behind this system is to simulate the factorial
;; operation using numeric entities
(define-system factorial
  ;; every second
  #:in [seconds clock/e]
  #:state [x 3]
  ;; this system is enabled as long as the clock's value is less
  ;; than 12 seconds 
  #:enabled? (< seconds 12)
  ;; query for all entities that have the component/archetype: Count
  ;; bind the entities to the list ents
  #:query ents (lookup Count)
  ;; compute factorial
  #:reduce fac
           (entity (gensym) (make-hasheq (list (cons 'Count 1))))
           (λ (a b) (entity (gensym) (make-hasheq (list (cons 'Count (* (get a 'Count) (get b 'Count)))))))
  ;; every iteration create a new entity 1 greater than the last
  ;; and print the current factorial
  #:post (displayln (get fac 'Count)) (sleep 1) (set! (add-entity! (list Count)) x) (+ x 1))

(module+ main
 (begin-recess
  #:systems factorial
  #:initialize (add-entity! (list Count)) (set-event! clock/e 0)
  #:stop-when factorial))
  