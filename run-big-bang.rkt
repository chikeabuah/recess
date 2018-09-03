#lang racket/base

(require recess 2htdp/universe 2htdp/image racket/hash)

(provide
 (all-defined-out)
 (all-from-out recess 2htdp/image))

;; adapt recess to use big bang
(struct big-bang-recess-world (pending-events current-recess-world last-output) #:transparent)

;; iterate through the graph until the world's termination conditions are fulfilled
(define (run/big-bang args)
  (match-define (list start-time stop-func current-events step-world current-world) args)
  (big-bang (big-bang-recess-world (make-immutable-hasheq) current-world #f)
    (on-tick (big-bang-step-world start-time current-events step-world) 1)
    (to-draw big-bang-draw-recess)
    (on-key big-bang-recess-key)
    (on-mouse big-bang-recess-mouse)
    (stop-when (big-bang-stop-condition stop-func))))

;; then `on-tick` takes those events, plus a freshly generated clock event and runs the
;; simulation, records the output in the world struct
(define (big-bang-step-world start-time current-events step-world)
  (λ (w)
    (match-define (big-bang-recess-world pe crw lo) w)
    ;; sync up with new things that have happened
    ;; right now this means merging the pending events into the current events
    (define events-so-far
      (hash-set
       (big-bang-recess-world-pending-events w)
       clock/e
       (- (current-seconds) (start-time))))
    (current-events (hash-union (current-events) events-so-far #:combine (λ (old new) new)))
    ;;then step
    (displayln "executing recess graph...")
    (step-world)
    ;; reset pending events and produce output
    ;; need to reset sink events too
    (struct-copy big-bang-recess-world w
                 [pending-events (make-immutable-hasheq)]
                 [last-output (list)])))

;; `on-key` and `on-mouse` events record something inside a custom made world struct
(define (big-bang-recess-key w key-event)
  (match-define (big-bang-recess-world pe crw lo) w)
  (struct-copy big-bang-recess-world w
               [pending-events (hash-set pe key/e key-event)]))

(define (big-bang-recess-mouse w x y mouse-event)
  (match-define (big-bang-recess-world pe crw lo) w)
  (struct-copy big-bang-recess-world w
               [pending-events (hash-set pe mouse/e mouse-event)]))

;; then `on-draw` will just pulls out the recess sink output
(define (big-bang-draw-recess w)
  (text "Hello" 24 "olive"))

(define (big-bang-stop-condition stop-func)
  (λ (w) (stop-func)))

