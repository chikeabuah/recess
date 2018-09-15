#lang racket/base
(require
  recess
  racket/match
  racket/fixnum
  racket/gui/base
  racket/class
  racket/hash
  lang/posn
  (prefix-in image: 2htdp/image)
  lux
  lux/chaos/gui
  lux/chaos/gui/val
  lux/chaos/gui/key)

(provide
 (all-defined-out)
 (all-from-out
  recess
  2htdp/image
  lang/posn))

;; iterate through the graph until the world's termination conditions are fulfilled
(define (run/lux-image args)
  (match-define (list start-time stop-func current-events step-world current-world) args)

  ;; then `on-tick` takes those events, plus a freshly generated clock event and runs the
  ;; simulation, records the output in the world struct
  (define (lux-step-world start-time current-events step-world w)
    (match-define (lux-recess-world g/v pe crw lo) w)
    ;; sync up with new things that have happened
    ;; right now this means merging the pending events into the current events
    (define events-so-far
      (hash-set
       pe
       clock/e
       (- (current-seconds) (start-time))))
    (current-events (hash-union (current-events) events-so-far #:combine (λ (old new) new)))
    ;; need to reset sink events
    (current-events
     (make-immutable-hasheq
      (map
       (λ (event-assoc)
         (match-define (cons ev val) event-assoc)
         (if (event:sink? ev)
             (cons ev (event-zero ev))
             event-assoc))
       (hash->list (current-events)))))
    ;;then step
    (displayln "executing recess graph...")
    (step-world)
    ;; get sink events, right now we only care about images
    (define image-outputs (hash-ref (current-events) image/e))
    ;; reset pending events and produce output
    (struct-copy lux-recess-world w
                 [pending-events (make-immutable-hasheq)]
                 [last-output image-outputs]))

  ;; `on-key` and `on-mouse` events record something inside a custom made world struct
  (define (lux-recess-key w key-event)
    (match-define (lux-recess-world g/v pe crw lo) w)
    (struct-copy lux-recess-world w
                 [pending-events (hash-set pe key/e key-event)]))

  (define (lux-recess-mouse w x y mouse-event)
    (match-define (lux-recess-world g/v pe crw lo) w)
    (struct-copy lux-recess-world w
                 [pending-events (hash-set pe mouse/e (make-posn x y))]))

  (define (lux-stop-condition stop-func)
    (not (stop-func)))

  (struct lux-recess-world
    (g/v pending-events current-recess-world last-output)
    #:methods gen:word
    [(define (word-fps w)
       1.0)
     (define (word-output w)
       (match-define (lux-recess-world g/v pe crw image-outputs) w)
       (define images (map car image-outputs))
       (define posns (map cdr image-outputs))
       (define output (image:place-images images posns (image:empty-scene 400 200)))
       (g/v output))
     (define (word-event w e)
       (match-define (lux-recess-world g/v pe crw lo) w)
       (define closed? #f)
       (cond
         [(eq? e 'close)
          #f]
         [(key-event? e)
          (lux-recess-key w e)]
         [(key-event? e)
          (lux-recess-key w e)]
         [else w]))
     (define (word-tick w)
       (define new-w (lux-step-world start-time current-events step-world w))
       (if (lux-stop-condition stop-func)
           new-w
           #f))])

  (call-with-chaos
   (make-gui)
   (λ () (fiat-lux (lux-recess-world (make-gui/val) (make-immutable-hasheq) current-world (list)))))

  #t)
