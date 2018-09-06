#lang racket/base

(require recess/run-big-bang)

(define (mouse-default m)
  (if (posn? m)
      m
      (make-posn 0 0)))
     
(define-system simple-follow-mouse
  #:in [seconds clock/e]
  #:in [mouse-posn mouse/e]
  #:pre m (displayln mouse-posn) (mouse-default mouse-posn)
  #:enabled? (< seconds 25)
  #:out [image/e (list (cons (circle 10 "solid" "blue") m))])

(module+ main
 (begin-recess
  #:systems simple-follow-mouse
  #:initialize #f
  #:stop (because #:systems simple-follow-mouse)
  #:run run/big-bang))
  