#lang racket/base

(require "recess-graph.rkt")

(provide
 (all-defined-out)
 (all-from-out "recess-graph.rkt"))

;; Tetris component types

(struct color (r g b)
  #:methods gen:inner-component-generic
  [(define (init-component inner-component-generic)
     (color 0 0 0))
   (define (randomize-component inner-component-generic)
     (color (random 256) (random 256)) (random 256))])

(struct posn (x y)
  #:methods gen:inner-component-generic
  [(define (init-component inner-component-generic)
     (posn 5 0))
   (define (randomize-component inner-component-generic)
     (posn 5 0))])

(struct counter (x)
  #:methods gen:inner-component-generic
  [(define (init-component inner-component-generic)
     (counter 0))
   (define (randomize-component inner-component-generic)
     (counter 0))])

(define shapes '(I J L O S T Z))

(struct shape (shape)
  #:methods gen:inner-component-generic
  [(define (init-component inner-component-generic)
     (list-ref shapes (random (length shapes))))
   (define (randomize-component inner-component-generic)
     (list-ref shapes (random (length shapes))))])


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