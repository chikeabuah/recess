#lang racket/base
(require recess/run-big-bang "fishy-game.rkt")

(module+ main
  (play-fishy-game! run/big-bang))
