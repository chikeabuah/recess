#lang racket/base
(require
  recess
  raart
  racket/match
  racket/format
  racket/fixnum
  racket/gui/base
  racket/class
  racket/hash
  lang/posn
  lux
  lux/chaos/gui
  lux/chaos/gui/val
  lux/chaos/gui/key
  struct-define)

(provide
 (all-defined-out)
 (all-from-out
  recess
  raart
  lang/posn))

(define rows 24)
(define cols 80)
(define world-rows (- rows 3))
(define world-cols cols)

;; iterate through the graph until the world's termination conditions are fulfilled
(define (run/lux-text args)
  (match-define (list start-time stop-func current-events step-world current-world) args)

  (struct obj (ox oy oc))
  (define-struct-define obj-define obj)

  (define (lux-step-world start-time current-events step-world w)
    (match-define (lux-recess-world g/v pe crw lo) w)
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

  (define (lux-stop-condition stop-func)
    (not (stop-func)))

  (struct lux-recess-world
    (g/v pending-events current-recess-world last-output)
    #:methods gen:word
    [(define (word-fps w)
       1.0)
     (define (word-output w)
       (match-define (lux-recess-world g/v pe crw image-outputs) w)
       (define chars (map car image-outputs))
       (define posns (map cdr image-outputs))
       (define objs
         (map
          (λ (char posn)
            (obj (posn-x posn) (posn-y posn) char))
          chars posns))
       (without-cursor
        (crop 0 cols 0 rows
              (vappend
               #:halign 'left
               (text "Press q to quit.")
               (for/fold ([c (blank world-cols world-rows)])
                         ([o (in-list objs)])
                 (obj-define o)
                 (place-at c oy ox (fg 'blue (char oc))))
               (text (~a "Recess+Lux+Raart"))))))
     (define (word-event w e)
       (match e
         [(screen-size-report _ _) w]
         ["q" #f]))
     (define (word-tick w)
       (define new-w (lux-step-world start-time current-events step-world w))
       (if (lux-stop-condition stop-func)
           new-w
           #f))])

  (call-with-chaos
   (make-raart)
   (λ ()
     (fiat-lux
      (lux-recess-world
       (make-gui/val)
       (make-immutable-hasheq)
       current-world
       (list)))))

  #t)
