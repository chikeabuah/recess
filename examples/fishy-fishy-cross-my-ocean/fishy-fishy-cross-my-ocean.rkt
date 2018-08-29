#lang racket/base

(require recess)

;; this example implements a RNG simulation of the children's playground game:
;; fishy fishy cross my ocean
;; the game follows the rules i found here:
;; https://www.cne-siar.gov.uk/media/5239/fishyfishy.pdf

(define SHARKS 5)
(define FISH 50)
(define OCEAN 100)

(define-component Shark)
(define-component Fish)
(define-component Seaweed)
(define-component Guess 0)

;; 
(define-system cross-my-ocean
  #:in [seconds clock/e]
  #:pre y (sleep 1)
  #:enabled? (< seconds 10)
  #:query e (lookup Shark Fish)
  #:map mapval (displayln (get e 'Guess)) (set! e (random OCEAN) 'Guess)
  #:out [sharks/e (filter Shark? mapval)])

(define-system shark-bite
  #:in [seconds clock/e]
  #:in [on-cross cross-my-ocean]
  #:in [sharks sharks/e]
  #:enabled? (< seconds 15)
  #:query e (lookup Fish)
  #:map _ (displayln (get e 'Count)) (set! e (* (get e 'Count) 5)))

(define-system seaweed-attack
  #:in [seconds clock/e]
  #:in [on-bite shark-bite]
  #:enabled? (< seconds 15)
  #:query e (lookup Seaweed)
  #:map _ (displayln (get e 'Count)) (set! e (* (get e 'Count) 5)))  

(module+ main
  (begin-recess
    #:systems cross-my-ocean shark-bite seaweed-attack
    #:initialize (add-entity! (list Shark Guess) SHARKS)
    (add-entity! (list Fish Guess) FISH)
    #:stop (because #:entities (eq? 0 (length (lookup Fish))))))
