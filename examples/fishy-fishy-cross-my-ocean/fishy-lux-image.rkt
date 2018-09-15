#lang racket/base
(require recess/run-lux-image "fishy-game.rkt")

(module+ main
  (play-fishy-game! run/lux-image))
