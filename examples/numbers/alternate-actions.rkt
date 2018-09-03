#lang racket/base

(require recess/run-big-bang)
     
(define-component Count 10) 

;; the idea in this example is to create a numeric pattern using a single entity
;; the pattern is to alternatingly subtract 5 and multiply by 5 to get the next number
;; for example:
;; 10,–5 5,×5 25,–5 20,×5 100,–5 95,×5 475, ...
;; the sequence is enforced by the dependency of multiply5 on subtract5
;; also they will not always be in sync because their termination conditions are different
(define-system subtract5
  #:in [seconds clock/e]
  #:enabled? (< seconds 10)
  #:query e (lookup Count)
  ;; every iteration increment subtract 5 from e
  #:map m (displayln (get e 'Count)) (set! e (- (get e 'Count) 5))
  #:out [image/e
         (list (cons (text (number->string (get (car m) 'Count)) 24 "olive") (make-posn 100 50)))])

(define-system multiply5
  #:in [seconds clock/e]
  ;; we can enforce dependencies through #:in
  ;; we can use the in to force multiply5 to always happen after subtractt5
  #:in [on-subtract subtract5]
  #:enabled? (< seconds 15)
  #:query e (lookup Count)
  ;; every iteration multiply e by 5
  #:map m (displayln (get e 'Count)) (set! e (* (get e 'Count) 5))
  #:out [image/e
         (list (cons (text (number->string (get (car m) 'Count)) 24 "olive") (make-posn 250 50)))])

(module+ main
 (begin-recess
  #:systems subtract5 multiply5
  #:initialize (add-entity! (list Count))
  #:stop (because #:systems multiply5)
  #:run run/big-bang))
  