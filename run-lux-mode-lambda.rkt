#lang racket/base
(require
  recess
  racket/match
  racket/flonum
  racket/hash
  racket/draw
  racket/class
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
(for-each
 (λ (ell i) 
   (add-sprite!/value db (format-symbol "ellipse-~a" (string->symbol (number->string i))) ell))
 ellipse10 (range 10))
(for-each
 (λ (txt i) 
   (add-sprite!/value db (format-symbol "text-~a" (string->symbol (number->string i))) txt))
 text10 (range 10))


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
    ;; sync up with new things that have happened
    ;; right now this means merging the pending events into the current events
    (define events-so-far
      (hash-set
       pe
       clock/e
       (- (current-seconds) (start-time))))
    (current-events (hash-union (current-events) events-so-far #:combine (λ (old new) new)))
    
    (define (reset-events reset?)
      (current-events
       (make-immutable-hasheq
        (map
         (λ (event-assoc)
           (match-define (cons ev val) event-assoc)
           (if (reset? ev)
               (cons ev (event-zero ev))
               event-assoc))
         (hash->list (current-events))))))
    
    ;; need to reset sink events (and key)
    (reset-events (λ (ev) (event:sink? ev)))
    ;;then step
    (displayln "executing recess graph...")
    (step-world)
    (reset-events (λ (ev) (eq? key/e ev)))
    ;; get sink events, right now we only care about images
    (define image-outputs (hash-ref (current-events) image/e))
    ;; reset pending events and produce output
    (struct-copy lux-recess-world w
                 [pending-events (make-immutable-hasheq)]
                 [last-output image-outputs]))


  ;; `on-key` and `on-mouse` events record something inside a custom made world struct
  (define (lux-recess-key w key-event)
    (match-define (lux-recess-world rs->d pe crw lo) w)
    (struct-copy lux-recess-world w
                 [pending-events (hash-set pe key/e key-event)]))

  (define (lux-recess-mouse w x y mouse-event)
    (match-define (lux-recess-world rs->d pe crw lo) w)
    (struct-copy lux-recess-world w
                 [pending-events (hash-set pe mouse/e (make-posn x y))]))

  (define (lux-stop-condition stop-func)
    (not (stop-func)))

  (struct lux-recess-world
    (g/v pending-events current-recess-world last-output)
    #:methods gen:word
    [(define (word-fps w)
       60.0)
     (define (word-output w)
       (match-define (lux-recess-world rendering-states->draw pe crw image-outputs) w)
       (define sprite-syms (map car image-outputs))
       (define posns (map cdr image-outputs))
       (define dynamic
         (map
          (λ (sprite-sym posn)
            (sprite
             (->fl (posn-x posn))
             (->fl (posn-y posn))
             (sprite-idx cdb sprite-sym) #:layer 3))
          sprite-syms
          posns))
       (define static (list))
       (define draw (rendering-states->draw lc static dynamic))
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
       (make-immutable-hasheq)
       current-world
       (list)))))

  #t)
