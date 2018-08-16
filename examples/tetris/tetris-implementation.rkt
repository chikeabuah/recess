#lang racket/base

(require "recess-graph.rkt")

(provide
 (all-defined-out)
 (all-from-out "recess-graph.rkt"))

;; Tetris component types

(struct color (r g b)
  #:methods gen:component-prototype-generic
  [(define (init-component component-prototype-generic)
     (color 0 0 0))])

(struct posn (x y)
  #:methods gen:component-prototype-generic
  [(define (init-component component-prototype-generic)
     (posn 5 0))])

(struct counter (x)
  #:methods gen:component-prototype-generic
  [(define (init-component component-prototype-generic)
     (counter 0))])

(define shapes '(I J L O S T Z))

(struct shape (shape)
  #:methods gen:component-prototype-generic
  [(define (init-component component-prototype-generic)
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