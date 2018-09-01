#lang racket/base

(require recess)

;; this example implements a RNG simulation of the children's playground game:
;; fishy fishy cross my ocean
;; the game follows the rules i found here:
;; https://www.cne-siar.gov.uk/media/5239/fishyfishy.pdf

#|
Fishy Fishy Cross My Ocean
Rules:
• Choose X players to be “sharks” they stand at one end of the playing area
• Another Y players are “fish” and they stand at the other side of the playing
area
• When the sharks call out “FISHY FISHY CROSS MY OCEAN” all the
fishy have to run to the opposite end of the playing area without getting tagged
by a shark. In this game we simulate being tagged by a random number match.
• If you are tagged, then you turn into “seaweed”. They must stand still on the
spot that they got tagged (same number), and try to touch/match other fish as they pass by. 
If they do, then they too turn into seaweed.
• The game ends when everyone has been turned into “seaweed”
Difficulty: To make it harder (end faster), you can have more sharks, or you could make the
playing area (random number range) smaller.
|#

(define SHARKS 5)
(define FISH 50)
;; random number range
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
  #:query e (lookup Player)
  ;; when there are only 5 players left it's all sharks and the rest are seaweed
  #:map mapval (displayln (get e 'Guess)) (set! e (random OCEAN) 'Guess) e
  #:out [shark-guesses/e (map (λ (e) (get e 'Guess)) (filter Shark? mapval))])

(define-system shark-bite
  #:in [seconds clock/e]
  #:in [on-cross cross-my-ocean]
  #:in [sharks shark-guesses/e]
  #:query e (lookup Fish)
  #:map _ (when (member (get e 'Guess) sharks) (- (+ e Seaweed) Fish Player)))

;; really these do the same thing except just in two different phases
(define-system seaweed-attack
  #:in [seconds clock/e]
  #:in [on-bite shark-bite]
  #:query e (lookup Fish)
  #:map _ (when (member (get e 'Guess) (lookup Seaweed)) (- (+ e Seaweed) Fish Player))) 

(module+ main
  (begin-recess
    #:systems cross-my-ocean shark-bite seaweed-attack
    #:initialize
    (add-entities! (list Shark Player Guess) SHARKS)
    (add-entities! (list Fish Player Guess) FISH)
    #:stop (because #:entities (eq? 0 (length (lookup Fish))))))
