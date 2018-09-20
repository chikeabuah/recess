#lang racket/base
(require recess/run-lux-mode-lambda "fishy-game.rkt")

(module+ main
  (play-fishy-game! run/lux-mode-lambda vis-players-as-sprites))
