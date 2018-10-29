#lang racket/base

(require recess/run-big-bang)

(provide
 (all-defined-out))


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
(define OCEAN 20)

(define-component Player)
(define-component Shark)
(define-component Fish)
(define-component Seaweed)
(define-component Guess 0)

(define-event shark-guesses/e)

(define (draw-players guesses player-type)
  (define shape
    (cond
      [(eq? player-type 'shark) (triangle 10 "solid" "violet")]
      [(eq? player-type 'fish) (circle 10 "solid" "blue")]
      [(eq? player-type 'seaweed) (square 10 "solid" "green")]))
  (map
   (λ (guess shape) (cons shape (make-posn (* guess 10) (* guess 10))))
   guesses
   (make-list (length guesses) shape)))

(define (draw-players-text guesses player-type)
  (define shape
    (cond
      [(eq? player-type 'shark) #\@]
      [(eq? player-type 'fish) #\>]
      [(eq? player-type 'seaweed) #\~]))
  (map
   (λ (guess shape) (cons shape (make-posn (* guess 10) (* guess 10))))
   guesses
   (make-list (length guesses) shape)))

(define (draw-players-mode-lambda guesses player-type)
  (map
   (λ (guess shape) (cons shape (make-posn (* guess 10) (* guess 10))))
   guesses
   (make-list (length guesses) player-type)))

;; writing this example made me think about wanting multiple queries in a system
;; or some kind of compound queries

(define-system cross-my-ocean
  #:in [seconds clock/e]
  #:query e (lookup Player)
  ;; when there are only 5 players left it's all sharks and the rest are seaweed
  #:map mapval #;(displayln (get e 'Guess)) (~>! e (random OCEAN) 'Guess) e
  #:out [shark-guesses/e (map (λ (en) (get en 'Guess)) (filter Shark? mapval))])

(define-system shark-bite
  #:in [seconds clock/e]
  #:in [on-cross cross-my-ocean]
  #:in [sharks shark-guesses/e]
  #:query e (lookup Fish)
  #:map _ (when (member (get e 'Guess) sharks) (minus (plus e Seaweed) Fish Player)))

;; really these do the same thing except just in two different phases
(define-system seaweed-attack
  #:in [seconds clock/e]
  #:in [on-bite shark-bite]
  #:query e (lookup Fish)
  #:map _ (when (member (get e 'Guess) (lookup Seaweed)) (minus (plus e Seaweed) Fish Player)))

(define-system vis-players
  #:in [seconds clock/e]
  #:in [on-seaweed seaweed-attack]
  #:out [image/e (draw-players (map (λ (en) (get en 'Guess)) (lookup Shark)) 'shark)]
  #:out [image/e (draw-players (map (λ (en) (get en 'Guess)) (lookup Fish)) 'fish)]
  #:out [image/e (draw-players (map (λ (en) (get en 'Guess)) (lookup Seaweed)) 'seaweed)])

(define-system vis-players-as-text
  #:in [seconds clock/e]
  #:in [on-seaweed seaweed-attack]
  #:out [image/e (draw-players-text (map (λ (en) (get en 'Guess)) (lookup Shark)) 'shark)]
  #:out [image/e (draw-players-text (map (λ (en) (get en 'Guess)) (lookup Fish)) 'fish)]
  #:out [image/e (draw-players-text (map (λ (en) (get en 'Guess)) (lookup Seaweed)) 'seaweed)])

(define-system vis-players-as-sprites
  #:in [seconds clock/e]
  #:in [on-seaweed seaweed-attack]
  #:out [image/e (draw-players-mode-lambda (map (λ (en) (get en 'Guess)) (lookup Shark)) 'shark)]
  #:out [image/e (draw-players-mode-lambda (map (λ (en) (get en 'Guess)) (lookup Fish)) 'fish)]
  #:out [image/e (draw-players-mode-lambda (map (λ (en) (get en 'Guess)) (lookup Seaweed)) 'seaweed)])
