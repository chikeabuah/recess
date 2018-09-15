#lang racket/base
(require recess "fishy-systems.rkt")

(provide play-fishy-game! (all-from-out "fishy-systems.rkt"))

(define (play-fishy-game! run-mode)
  (begin-recess
    #:systems cross-my-ocean shark-bite seaweed-attack vis-players
    #:initialize
    (add-entities! (list Shark Player Guess) SHARKS)
    (add-entities! (list Fish Player Guess) FISH)
    #:stop (because #:entities (eq? 0 (length (lookup Fish))))
    #:run run-mode))
