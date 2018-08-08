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
  "red")

(define (random-shape)
  "O")

(define (unit-block)
  0)

(define (tetro-to-blocks e)
  e)

(define (valid-ccw? tetro cs)
  #t)

(define (valid-cw? tetro cs)
  #t)

(define (vacant-down? tetro cs)
  #t)

(define (vacant-right? tetro cs)
  #t)

(define (vacant-left? tetro cs)
  #t)

(define (rotate90 tetro)
  #t)