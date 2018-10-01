#lang racket/base

(require recess/run-lux-mode-lambda)

(provide (all-defined-out))

;; helpers

;; enemy grid

(define top-left (make-posn 40 40))
(define horizontal (map (λ (n) (* n 80)) (cdr (range 9))))
(define top-row-temp (make-list 8 top-left))
(define top-row
  (map
   (λ (p x-offset) (make-posn (+ x-offset (posn-x p)) (posn-y p)))
   top-row-temp
   horizontal))
(define (offset-row row y-offset)
  (map
   (λ (pos)
     (make-posn (posn-x pos) (+ (posn-y pos) y-offset)))
   row))

(define enemy-matrix
  (map
   (λ (row y-offset) (offset-row row y-offset))
   (make-list 5 top-row)
   (map (λ (n) (* n 80)) (cdr (range 6)))))

(define (draw-entities posns sprite-sym)
  (map
   (λ (position) (cons sprite-sym position))
   posns))

(define (move-player! player key)
  (define k
    (if (and key (not (pair? key)))
        key
        #f))
  (define offset 
    (cond 
      [(eq? k 'left) -20]
      [(eq? k 'right) 20]
      [else 0]))
  (define player-posn (get player 'Position))
  (define new-posn (make-posn (+ (posn-x player-posn) offset) (posn-y player-posn)))
  (~>! player new-posn 'Position))

(define (move-bullet pos)
  (make-posn (posn-x pos) (- (posn-y pos) 5)))

(define (h-align-shot bullet key player)
  (define player-posn (get player 'Position))
  (when (and key (eq? (key-event-code key) #\s))
    (~>! (add-entity! bullet) player-posn 'Position)))

(define (get-entity-posns ents)
  (map
   (λ (ent) (get ent 'Position))
   ents))

(define (distance a b)
  (sqrt
   (+
    (expt (- (posn-x a) (posn-x b)) 2)
    (expt (- (posn-y a) (posn-y b)) 2))))

(define (close-enough? thresh pos posns)
  (define poslst (make-list (length posns) pos))
  (define distances (map distance poslst posns))
  (ormap (λ (d) (< d thresh)) distances))

(define (move-enemy-h en)
  (begin
    (define current-max-offset (get en 'MaxOffset))
    (define current-offset (get en 'CurrentOffset))
    (define axis (get en 'Axis))
    (define polarity (get en 'Polarity))
    (define pos (get en 'Position))
    (define fw (get en 'FirstWall))
    (define switch? (<= current-max-offset current-offset))
    (when switch?
      (set! polarity (not polarity))
      (set! current-offset 0))
    (displayln polarity)
    ;; we hit our first wall: double max offset
    (when (and switch? (not fw))
      (set! current-max-offset (* current-max-offset 2))
      (set! fw (not fw)))
    (set! current-offset (+ current-offset 2))
    (if polarity   
        (set! pos (make-posn (+ (posn-x pos) 2) (posn-y pos)))
        (set! pos (make-posn (- (posn-x pos) 2) (posn-y pos))))
    (~~>! en (make-immutable-hasheq
              (list
               (cons 'MaxOffset current-max-offset)
               (cons 'Polarity polarity)
               (cons 'FirstWall fw)
               (cons 'Position pos)
               (cons 'CurrentOffset current-offset))))))