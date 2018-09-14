#lang racket/base
(require recess/run-big-bang "fishy-systems.rkt")

(module+ main
  (begin-recess
    #:systems cross-my-ocean shark-bite seaweed-attack vis-players
    #:initialize
    (add-entities! (list Shark Player Guess) SHARKS)
    (add-entities! (list Fish Player Guess) FISH)
    #:stop (because #:entities (eq? 0 (length (lookup Fish))))
    #:run run/big-bang))
