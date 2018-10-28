#lang racket/base

(require
  recess
  2htdp/universe
  2htdp/image
  lang/posn
  racket/hash)

(provide
 (all-defined-out)
 (all-from-out
  recess
  2htdp/image
  lang/posn))

;; adapt recess to use big bang
(struct big-bang-recess-world (pending-events current-recess-world last-output) #:transparent)

;; iterate through the graph until the world's termination conditions are fulfilled
(define (run/big-bang args)
  (match-define (list start-time stop-func current-events step-world current-world) args)
  (big-bang (big-bang-recess-world (make-vector EVMAX #f) current-world (list))
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
    (vector-set!
     pe
     (hash-ref event-registry clock/e)
     (- (current-seconds) (start-time)))
    (current-events
     (for/vector ([new pe] [old (current-events)] ) (if new new old)))
    
    (define (reset-events reset?)
      (for-each
       (λ (event-assoc)
         (match-define (cons ev idx) event-assoc)
         (when (reset? ev)
           (vector-set! (current-events) idx (event-zero ev))))
       (hash->list event-registry)))
    
    ;; need to reset sink events (and key)
    (reset-events (λ (ev) (event:sink? ev)))
    ;;then step
    ;(displayln "executing recess graph...")
    (step-world)
    (reset-events (λ (ev) (eq? key/e ev)))
    ;; get sink events, right now we only care about images
    (define image-outputs (vector-ref (current-events) (hash-ref event-registry image/e)))
    ;; reset pending events and produce output
    (struct-copy big-bang-recess-world w
                 [pending-events (make-vector EVMAX #f)]
                 [last-output image-outputs])))

;; `on-key` and `on-mouse` events record something inside a custom made world struct
(define (big-bang-recess-key w key-event)
  (match-define (big-bang-recess-world pe crw lo) w)
  (struct-copy big-bang-recess-world w
               [pending-events (hash-set pe key/e key-event)]))

(define (big-bang-recess-mouse w x y mouse-event)
  (match-define (big-bang-recess-world pe crw lo) w)
  (struct-copy big-bang-recess-world w
               [pending-events (hash-set pe mouse/e (make-posn x y))]))

;; then `on-draw` will just pulls out the recess sink output
(define (big-bang-draw-recess w)
  (match-define (big-bang-recess-world pe crw image-outputs) w)
  (define images (map car image-outputs))
  (define posns (map cdr image-outputs))
  (place-images images posns (empty-scene 400 200)))

(define (big-bang-stop-condition stop-func)
  (λ (w) (stop-func)))

