#lang racket/base

(require recess)
     
(define-event clock/e)

;; the idea behind this system is to synchronize
;; a system's internal state with a time clock
(define-system sync-to-clock
  ;; every second
  #:in [clock/e 'change]
  ;; there is a binding called x in our state
  ;; initially it has the value of 5
  #:state [x 5]
  ;; this system is enabled as long as x is less than 15
  #:enabled? (< x 15)
  ;; every iteration increment x by 1
  #:post (add1! x))

(module+ main
 (begin-recess
  #:systems sync-to-clock
  #:initialize (set-event! clock/e 0)
  #:stop-when all-systems-stop))
  