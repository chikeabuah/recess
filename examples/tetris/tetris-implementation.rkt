#lang racket/base

(require "recess-graph.rkt")

(provide
 (all-defined-out)
 (all-from-out "recess-graph.rkt"))

;; Tetris component types

(struct color (r g b)
  #:methods gen:component-generic
  [(define (init-component component-generic)
     (color 0 0 0))
   (define (randomize-component component-generic)
     (color (random 256) (random 256)) (random 256))])

(struct posn (x y)
  #:methods gen:component-generic
  [(define (init-component component-generic)
     (posn 5 0))
   (define (randomize-component component-generic)
     (posn 5 0))])

(struct counter (x)
  #:methods gen:component-generic
  [(define (init-component component-generic)
     (counter 0))
   (define (randomize-component component-generic)
     (counter 0))])

(define I J L O S T Z)
(struct shape (x)
  #:methods gen:component-generic
  [(define (init-component component-generic)
     )
   (define (randomize-component component-generic)
     )])

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