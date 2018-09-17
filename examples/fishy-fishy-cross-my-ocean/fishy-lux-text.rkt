#lang racket/base
(require recess/run-lux-text "fishy-game.rkt")

(module+ main
  (play-fishy-game! run/lux-text vis-players-as-text))
