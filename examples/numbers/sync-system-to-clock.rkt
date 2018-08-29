#lang racket/base

(require recess)

;; the idea behind this system is to synchronize
;; a system's internal state with a time clock
(define-system sync-to-clock
  #:in [seconds clock/e]
  ;; there is a binding called x in our state
  ;; initially it has the value of 5
  #:state [x 5]
  ;; every iteration increment x by 1
  #:pre _ (displayln x) (sleep 1) (displayln seconds) (+ x 1)
  ;; this system is enabled as long as x is less than 15
  #:enabled? (< x 15))

(module+ main
 (begin-recess
  #:systems sync-to-clock
  #:initialize (set-event! clock/e 0)
  #:stop (because #systems sync-to-clock)))
  