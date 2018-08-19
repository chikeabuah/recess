#lang racket/base

(require recess)

(struct counter (x)
  #:methods gen:component-prototype-generic
  [(define (init-component component-prototype-generic)
     (counter 0))])
     
(define-component Count counter) 
(define-event clock/e)

;; the idea in this example is to create a numeric pattern
;; using a single entity
;; the pattern is to alternatingly subtract 5 and multiply by 5 to get the next number
;; for example:
;; 10,–5 5,×5 25,–5 20,×5 100,–5 95,×5 475, ...
;; the sequence is enforced by the dependency of multiply5 on subtract5
;; also they will not always be in sync because their termination conditions are different
(define-system subtract5
  ;; every second
  #:in [seconds clock/e]
  #:query e (Count)
  #:enabled? (< seconds 10)
  ;; every iteration increment subtract 5 from e
  #:post (set! e (- e 5)))

  (define-system multiply5
  ;; every second
  #:in [seconds clock/e]
  #:in [on-subtract subtract5]
  #:query e (Count)
  #:enabled? (< seconds 15)
  ;; every iteration multiply e by 5
  #:post (set! e (* e 5)))

(module+ main
 (begin-recess
  #:systems subtract5 multiply5
  #:initialize (add-entity! (Count)) (set-event! clock/e 0)
  #:stop-when multiply5))
  