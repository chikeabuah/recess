#lang racket/base

(require recess/run-lux-mode-lambda racket/set)

(define PARTICLES 500)

(define GRIDN 10)

;; v then h
(define GRID (make-hash))

(for ([v-idx (in-range GRIDN)])
  (for ([h-idx (in-range GRIDN)])
    (hash-set! GRID (cons v-idx h-idx) (mutable-seteq))))

(define (draw-entities pcs sprite-ref)
  (map
   (λ (pc) (cons (string->symbol (string-append (cdr pc) "-" sprite-ref)) (car pc)))
   pcs))

(define (distance a b)
  (sqrt
   (+
    (expt (- (posn-x a) (posn-x b)) 2)
    (expt (- (posn-y a) (posn-y b)) 2))))

(define (rotate fv theta)
  (cons
   (- (* (car fv) (cos theta)) (* (cdr fv) (sin theta)))
   (+ (* (car fv) (sin theta)) (* (cdr fv) (cos theta)))))

(define (collide! p)
  ; p is a particle entity vector
  (define px (posn-x (get p 'Position)))
  (define py (posn-y (get p 'Position)))
  (define idx (get p 'GameID))
  
  (define ps (lookup-by-indices (set->list (hash-ref GRID (cons (getidx py) (getidx px))))))
  (for ([op (in-list ps)])
    (when (and
           (not (eq? (get p 'GameID) (get op 'GameID)))
           (> 150 (distance (get p 'Position) (get op 'Position))))
      (begin
        (define m1 (get p 'Mass))
        (define m2 (get op 'Mass))
        (define px (posn-x (get p 'Position)))
        (define py (posn-y (get p 'Position)))
        (define opx (posn-x (get op 'Position)))
        (define opy (posn-y (get op 'Position)))
        (define pfx (get p 'SpeedX))
        (define pfy  (get p 'SpeedY))
        (define opfx  (get op 'SpeedX))
        (define opfy  (get op 'SpeedY))
        (define theta
          (* -1
             (atan
              (if (eq? 0 (- opy py)) (add1 (- opy py)) (- opy py))
              (if (eq? 0 (- opx px)) (add1 (- opx px)) (- opx px)))))
        (define fv1 (rotate (cons pfx pfy) theta))
        (define fv2 (rotate (cons opfx opfy) theta))
        (define u1
          (rotate
           (cons
            (+ (* (car fv1) (/ (- m1 m2) (+ m1 m2))) ( * 2 (car fv2) (/ m2 (+ m1 m2))))
            (cdr fv1))
           (* -1 theta)))
        (define u2
          (rotate
           (cons
            (+ (* (car fv2) (/ (- m2 m1) (+ m1 m2))) ( * 2 (car fv1) (/ m1 (+ m1 m2))))
            (cdr fv2))
           (* -1 theta)))
        (begin
          (~>! p (random-color) 'Color)
          (~>! p (car u1) 'SpeedX)
          (~>! p (cdr u1) 'SpeedY)
          (~>! op (random-color) 'Color)
          (~>! op (car u2) 'SpeedX)
          (~>! op (cdr u2) 'SpeedY))
        #;(~~>! p (make-immutable-hasheq
                 (list
                  (cons 'Color (random-color))
                  (cons 'SpeedX (car u1))
                  (cons 'SpeedY (cdr u1)))))
        #;(~~>! op (make-immutable-hasheq
                  (list
                   (cons 'Color (random-color))
                   (cons 'SpeedX (car u2))
                   (cons 'SpeedY (cdr u2)))))))))      

(define (move-bounded-particle! particle W H G)
  (define px (posn-x (get particle 'Position)))
  (define py (posn-y (get particle 'Position)))
  (define vx (get particle 'SpeedX))
  (define vy (get particle 'SpeedY))
  (define r (get particle 'Size))
  (define idx (get particle 'GameID))
  (when (<= (- px r) 0)
    (set! px r))
  (when (>= (+ px r) W)
    (set! px (- W r)))  
  (when (or (and (<= (- px r) 0) (< vx 0)) (and (>= (+ px r) W) (> vx 0)))
    (set! vx (* vx -1)))
  (when (<= (- py r) 0)
    (set! py r))
  (when (>= (+ py r) H)
    (set! py (- H r)))  
  (when (or (and (<= (- py r)  0) (< vy 0)) (and (>= (+ py r) W) (> vy 0)))
    (set! vy (* vy -1)))
  (define new-x (inexact->exact (truncate (+ px vx))))
  (define new-y (inexact->exact (truncate (+ py vy))))
  (when (>= new-y H) (set! new-y H))
  (when (>= new-x W) (set! new-x W))
  (define new-posn (make-posn new-x new-y))
  ;; unset old grid pos
  (set-remove! (hash-ref G (cons (getidx py) (getidx px))) idx)
  ;; set new grid pos
  (set-add!
   (hash-ref G (cons
                (getidx new-y)
                (getidx new-x)))
   idx)
  (~~>! particle (make-immutable-hasheq
                  (list
                   (cons 'SpeedX vx)
                   (cons 'SpeedY vy)
                   (cons 'Position new-posn)))))

;; these need to be equal for a square grid 
(define W 3000)
(define H 3000)
(define S 50)
(define-component Particle)
(define-component Position (make-posn 1000 1000))
(define-component Shape)
(define-component Color)
(define-component Mobile)
(define-component Size 25)
(define-component Effect)
(define-component BoundingX)
(define-component BoundingY)
(define-component Mass)
(define-component SpeedX 5)
(define-component SpeedY 5)
(define-component GameID)

;; let's try using grid detection to speed up collision detection
;; i feel like this approach has the potential to be faster because
;; we will do all the allocation up front
;; also probably easier to implement/understand

;; might want to try both and compare if we have the time

;; calculate which grid square this particle is in
;; call 2wice for x/y
(define (getidx pos)
  (define grids (/ W GRIDN))
  ;; sp case for when a particle falls on the borderline
  ;; put it in the grid box above/left
  (define border? (and (eq? 0 (modulo pos grids)) (not (eq? pos 0))))
  (define p (if (eq? pos 0)
                0
                (truncate (/ pos grids))))
  (if border? (- p 1) p))

(define-system particle-collision
  #:query particle (lookup Particle Position Mobile)
  #:map _ (collide! particle))

(define-system move-particles
  #:in [on-collide particle-collision]
  #:query particle (lookup Particle Position Mobile)
  #:map pos (move-bounded-particle! particle W H GRID))

(define-system render-particles
  #:in [on-move move-particles]
  #:query particle (lookup Particle Position Color)
  #:map pc (cons (get particle 'Position) (get particle 'Color))
  #:out [image/e (draw-entities pc "ellipse")])

(begin-recess
  #:systems
  render-particles  
  move-particles
  particle-collision
  #:initialize
  ;; add particles
  (for ([idx (in-range PARTICLES)])
    (add-entity!
     (list Particle Mobile Size
           (let* ([r (random 2)]
                  [s (if (eq? r 0) 1 -1)])
             (copy-component 'SpeedX (* s (add1 (random S)))))
           (let* ([r (random 2)]
                  [s (if (eq? r 0) 1 -1)])
             (copy-component 'SpeedY (* s (add1 (random S)))))
           (copy-component 'GameID EIDX)
           (copy-component 'Color (random-color))
           (copy-component 'Mass (add1 (random 20)))
           (let* ([w (random W)]
                  [h (random H)]
                  [w-idx (getidx w)]
                  [h-idx (getidx h)])
             (begin
               (set-add! (hash-ref GRID (cons h-idx w-idx)) EIDX) 
               (copy-component 'Position (make-posn (random W) (random H))))))))
  #:stop #f
  #:run run/lux-mode-lambda)






