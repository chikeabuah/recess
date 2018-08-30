#lang racket/base

(require recess)

;; this example implements a RNG simulation of the children's playground game:
;; fishy fishy cross my ocean
;; the game follows the rules i found here:
;; https://www.cne-siar.gov.uk/media/5239/fishyfishy.pdf

(define SHARKS 5)
(define FISH 50)
(define OCEAN 10)

(define-component Player)
(define-component Shark)
(define-component Fish)
(define-component Seaweed)
(define-component Guess 0)

(define-event shark-guesses/e)

;; writing this example made me think about wanting multiple queries in a system
;; or some kind of compound queries

(define-system cross-my-ocean
  #:in [seconds clock/e]
  #:pre y (sleep 1)
  #:query e (lookup Player)
  #:map mapval (displayln (get e 'Guess)) (set! e (random OCEAN) 'Guess) e
  #:out [shark-guesses/e (map (λ (e) (get e 'Guess)) (filter Shark? mapval))])

(define-system shark-bite
  #:in [seconds clock/e]
  #:in [on-cross cross-my-ocean]
  #:in [sharks shark-guesses/e]
  #:query e (lookup Fish)
  #:map _
  (displayln e)
  (displayln "sharks") (displayln sharks)
  (when (member (get e 'Guess) sharks) (- (+ e Seaweed) Fish Player)))

;; really these do the same thing except just in two different phases
(define-system seaweed-attack
  #:in [seconds clock/e]
  #:in [on-bite shark-bite]
  #:query e (lookup Fish)
  #:map _
  (displayln (get e 'Guess))
  (when (member (get e 'Guess) (lookup Seaweed)) (- (+ e Seaweed) Fish Player))) 

(module+ main
  (begin-recess
    #:systems cross-my-ocean shark-bite seaweed-attack
    #:initialize
    (add-entities! (list Shark Player Guess) SHARKS)
    (add-entities! (list Fish Player Guess) FISH)
    #:stop (because #:entities (eq? 0 (length (lookup Fish))))))
