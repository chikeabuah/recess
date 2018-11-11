#lang racket/base

(require recess/run-lux-mode-lambda)

(provide (all-defined-out))

;; helpers

(define (draw-entities posns sprite-sym)
  (map
   (λ (position) (cons sprite-sym position))
   posns))

(define (distance a b)
  (sqrt
   (+
    (expt (- (posn-x a) (posn-x b)) 2)
    (expt (- (posn-y a) (posn-y b)) 2))))

(define (rotate fv theta)
  (cons
   (- (* (car fv) (cos theta)) (* (cdr fv) (sin theta)))
   (+ (* (car fv) (sin theta)) (* (cdr fv) (cos theta)))))

(define (collide! p ps)
  ; p is a particle entity vector
  ; ps is a list of refs to these
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
        (define theta (* -1 (atan (- opy py) (- opx px))))
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
        (~~>! p (make-immutable-hasheq
                  (list
                   (cons 'SpeedX (car u1))
                   (cons 'SpeedY (cdr u1)))))
        (~~>! op (make-immutable-hasheq
                  (list
                   (cons 'SpeedX (car u2))
                   (cons 'SpeedY (cdr u2)))))))))      

(define (move-bounded-particle! particle W H)
  (define px (posn-x (get particle 'Position)))
  (define py (posn-y (get particle 'Position)))
  (define vx (get particle 'SpeedX))
  (define vy (get particle 'SpeedY))
  (define r (get particle 'Size))
  (when (<= (- px r) 0)
    (set! px r))
  (when (>= (+ px r) W)
    (set! px (- W r)))  
  (when (or (and (<= (- px r) 0) (< vx 0)) (and (>= (+ px r) W) (> vx 0)))
    (set! vx (* vx -1)))
  (when (<= (- py r) 0)
    (set! py r))
  (when (>= (+ py r) H)
    (set! px (- H r)))  
  (when (or (and (<= (- py r)  0) (< vy  0)) (and (>= (+ py r) W) (> vy 0)))
    (set! vy (* vy -1)))
  (define new-posn (make-posn (+ px vx) (+ py vy)))
  (~~>! particle (make-immutable-hasheq
                  (list
                   (cons 'SpeedX vx)
                   (cons 'SpeedY vy)
                   (cons 'Position new-posn)))))
