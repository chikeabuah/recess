#lang racket/base
(require
  recess
  racket/match
  racket/flonum
  racket/hash
  racket/draw
  racket/class
  racket/vector
  lang/posn
  #;(prefix-in image: 2htdp/image)
  lux
  lux/chaos/gui
  lux/chaos/gui/val
  lux/chaos/gui/key
  mode-lambda
  mode-lambda/static
  mode-lambda/backend/gl
  pict)

(provide
 (all-defined-out)
 (all-from-out
  recess
  lux
  lux/chaos/gui
  lux/chaos/gui/val
  lux/chaos/gui/key
  lang/posn))

(define COLORS (send the-color-database get-names))

(define (random-color)
  (list-ref COLORS (random (length COLORS))))

(define rc random-color)

;;;
;;; SIZES
;;;

(define W 720)
(define H 640)
(define W/2 (/ W 2.))
(define H/2 (/ H 2.))

;;;
;;; BITMAPS
;;;

(define fish-p
  (standard-fish 10 5 #:color "blue"))

(define shark-p
  (standard-fish 10 5 #:color "red"))

(define seaweed-p
  (standard-fish 10 5  #:color "green"))

(define score-p
  (text "SCORE:" (cons (send the-color-database find-color "white") null) 20))

(define (ellipse-maker c)
  (disk 40 #:color (car c) #:border-color (cdr c) #:border-width 5))

;; make 10 random looking ellipses
(define ellipse10
  (map
   ellipse-maker
   (map
    (λ (p) (cons (list-ref COLORS (car p)) (list-ref COLORS (cdr p)))) 
    (map
     (λ (n m) (cons (random n) (random m)))
     (make-list 10 (length COLORS))
     (make-list 10 (length COLORS))))))

;; make numbers 0-9
(define text10
  (map
   (λ (n)
     (text (number->string n) (cons (send the-color-database find-color "white") null) 20))
   (range 10)))

;;;
;;; SPRITES
;;;

(define db (make-sprite-db))
(add-sprite!/value db 'fish  fish-p)
(add-sprite!/value db 'shark shark-p)
(add-sprite!/value db 'seaweed seaweed-p)
(add-sprite!/value db 'score score-p)
(for ([ell ellipse10] [i (range 10)])
  (add-sprite!/value db (format-symbol "ellipse-~a" (string->symbol (number->string i))) ell))
(for ([txt text10] [i (range 10)])
  (add-sprite!/value db (format-symbol "text-~a" (string->symbol (number->string i))) txt))
(define cdb (compile-sprite-db db))

;;;
;;; LAYERS
;;;

(define bugl (layer W/2 H/2))    ; gray:       layer 0 ; too see bugs in GL
(define bgl  (layer W/2 H/2))    ; background: layer 1
(define ml   (layer W/2 H/2))    ; middle:     layer 2
(define fgl  (layer W/2 H/2))    ; foreground: layer 3
(define lc   (vector bugl bgl ml fgl)) ; layer config

;; iterate through the graph until the world's termination conditions are fulfilled
(define (run/lux-mode-lambda args)
  (match-define (list start-time stop-func current-events step-world current-world) args)

  ;; then `on-tick` takes those events, plus a freshly generated clock event and runs the
  ;; simulation, records the output in the world struct
  (define (lux-step-world start-time current-events step-world w)
    (match-define (lux-recess-world rs->d pe crw lo) w)
    ;;beginning run
    (dte "beginning step")
    ;; sync up with new things that have happened
    ;; right now this means merging the pending events into the current events
    (vector-set!
     pe
     (hash-ref event-registry clock/e)
     (- (current-inexact-milliseconds) (start-time)))
    (current-events
     (for/vector ([new pe] [old (current-events)] ) (if new new old)))
    (define (reset-events reset?)
      (define (reset ev idx)
        (when (reset? ev)
          (vector-set! (current-events) idx (event-zero ev))))
      (for ([(k v) (in-hash event-registry)])
        (reset k v)))
    ;; need to reset sink events (and key)
    (reset-events (λ (ev) (event:sink? ev)))
    ;;then step
    ;(displayln "executing recess graph...")
    (step-world)
    (reset-events (λ (ev) (eq? key/e ev)))
    ;; get sink events, right now we only care about images
    (define image-outputs (vector-ref (current-events) (hash-ref event-registry image/e)))
    ;; reset pending events and produce output
    (dte "done resetting events")
    (for ([ev pe]
          [idx (in-naturals)])
      (begin (vector-set! pe idx #f)))
    (begin0 (struct-copy lux-recess-world w
                         [pending-events pe]
                         [last-output image-outputs])
      (dte "end step")))

  ;; `on-key` and `on-mouse` events record something inside a custom made world struct
  (define (lux-recess-key w key-event)
    (match-define (lux-recess-world rs->d pe crw lo) w)
    (vector-set! pe (hash-ref event-registry key/e) key-event)
    w)

  (define (lux-recess-mouse w x y mouse-event)
    (match-define (lux-recess-world rs->d pe crw lo) w)
    (vector-set! pe (hash-ref event-registry mouse/e) (make-posn x y))
    w)

  (define (lux-stop-condition stop-func)
    (not (stop-func)))

  (struct lux-recess-world
    (g/v pending-events current-recess-world last-output)
    #:methods gen:word
    [(define (word-fps w)
       60.0)
     (define (word-output w)
       (match-define (lux-recess-world rendering-states->draw pe crw image-outputs) w)
       (dte "starting rendering")
       (define dynamic
         (for/list ([io (in-list image-outputs)])
           (match-define (cons sym (posn x y)) io)           
           (sprite (->fl x) (->fl y) (sprite-idx cdb sym)
                   #:layer 3)))
       (define draw
         (rendering-states->draw lc '() dynamic))
       (dte "finished rendering")
       draw)
     (define (word-event w e)
       (match-define (lux-recess-world rs->d pe crw lo) w)
       (define closed? #f)
       (cond
         [(eq? e 'close)
          #f]
         [(key-event? e)
          (lux-recess-key w e)]
         [else w]))
     (define (word-tick w)
       (define new-w (lux-step-world start-time current-events step-world w))
       (if (lux-stop-condition stop-func)
           new-w
           #f))])

  (call-with-chaos
   (make-gui #:mode gui-mode)
   (λ ()
     (fiat-lux
      (lux-recess-world
       (stage-draw/dc cdb W H (vector-length lc))
       (make-vector EVMAX #f)
       current-world
       (list)))))

  #t)
