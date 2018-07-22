#lang racket/base

(provide
 (all-defined-out))

;; Tetris component types

(struct color (r g b))
(struct posn (x y))
(struct counter (x))

;; (define-enum Block I J L O S T Z)
(struct shape (x))

(define (random-color)
  "red"
  )

(define (random-shape)
  "O"
  )

(define (unit-block)
  0
  )
