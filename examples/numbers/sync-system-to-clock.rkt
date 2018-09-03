#lang racket/base

(require recess/run-big-bang)

;; the idea behind this system is to synchronize
;; a system's internal state with a time clock
(define-system sync-to-clock
  #:in [seconds clock/e]
  ;; there is a binding called x in our state
  ;; initially it has the value of 5
  #:state [x 5]
  ;; every iteration increment x by 5
  #:pre _ (displayln x) (displayln seconds) (+ x 5)
  ;; this system is enabled as long as x is less than 100
  #:enabled? (< x 100)
  #:out [image/e (list (cons (circle 20 "solid" "blue") (make-posn (* x 3) (* x 2))))])

(module+ main
 (begin-recess
  #:systems sync-to-clock
  #:initialize #f
  #:stop (because #:systems sync-to-clock)
  #:run run/big-bang))
  