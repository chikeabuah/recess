#lang racket/base

(require recess/run-lux-mode-lambda)

(provide (all-defined-out))

;; helpers

;; enemy grid
(define top-row
  (map
   (λ (p x-offset) (make-posn (+ x-offset (posn-x p)) (posn-y p)))
   (make-list 8 (make-posn 40 40))
   (map (λ (n) (* n 80)) (cdr (range 9)))))

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

(define (draw-number n)
  (define num (get n 'Score))
  (define str (number->string num))
  (define lst (filter (λ (c) (not (equal? c ""))) (string-split str "")))
  (define offsets (map (λ (i) (make-posn (+ (* 10 i) 110) 40)) (range (length lst))))
  (define res (map
   (λ (ch pos) (cons (format-symbol "text-~a" (string->symbol ch)) pos))
   lst offsets))
  (displayln res)
  (define r (append (list (cons 'score (make-posn 60 40))) res))
  r)

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

(define (move-friendly-bullet pos)
  (make-posn (posn-x pos) (- (posn-y pos) 10)))

(define (move-unfriendly-bullet pos)
  (make-posn (posn-x pos) (+ (posn-y pos) 5)))

(define (shoot-from-enemy en bullet)
  (define rate (get en 'FireDelay))
  (define en-posn (get en 'Position))
  (define a (random rate))(define b (random rate))
  (define shoot? (eq? a b))
  (when shoot?
    (~>! (add-entity! bullet) en-posn 'Position)))

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
      (set! pos (make-posn (posn-x pos) (+ (posn-y pos) 5)))
      (set! polarity (not polarity))
      (set! current-offset 0))
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
