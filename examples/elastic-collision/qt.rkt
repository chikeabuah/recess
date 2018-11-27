#lang racket/base

(require lang/posn)


(provide
 (all-defined-out))

(define MAXOBJECTS 10)
(define MAXLEVELS 4)

(struct bound (psn w h))
(struct bo (bnd data))
(struct quadtree (level bounds objects nodes) #:mutable)

(define (split! qt)
  (define nextlevel (add1 (quadtree-level qt)))
  (define subwidth (round (/ (bound-w (quadtree-bounds qt)) 2)))
  (define subheight (round (/ (bound-h (quadtree-bounds qt)) 2)))
  (define x (posn-x (bound-psn (quadtree-bounds qt))))
  (define y (posn-y (bound-psn (quadtree-bounds qt))))
  (define nodes (quadtree-nodes qt))

  (define top-left
    (quadtree nextlevel
              (bound (make-posn x y) subwidth subheight)
              (list)
              (vector #f #f #f #f)))

  (define top-right
    (quadtree nextlevel
              (bound (make-posn (+ x subwidth) y) subwidth subheight)
              (list)
              (vector #f #f #f #f)))

  (define bottom-left
    (quadtree nextlevel
              (bound (make-posn x (+ y subheight)) subwidth subheight)
              (list)
              (vector #f #f #f #f)))

  (define bottom-right
    (quadtree nextlevel
              (bound (make-posn (+ x subwidth) (+ y subheight)) subwidth subheight)
              (list)
              (vector #f #f #f #f)))

  (vector-set! nodes 0 top-left)
  (vector-set! nodes 1 top-right)
  (vector-set! nodes 2 bottom-left)
  (vector-set! nodes 3 bottom-right))

(define (get-index qt b)
  (define index -1)
  (define qt-w (bound-w (quadtree-bounds qt)))
  (define qt-h (bound-h (quadtree-bounds qt)))
  (define qt-x (posn-x (bound-psn (quadtree-bounds qt))))
  (define qt-y (posn-y (bound-psn (quadtree-bounds qt))))
  (define vmid (+ qt-x (/ qt-w 2)))
  (define hmid (+ qt-y (/ qt-h 2)))

  (define w (bound-w b))
  (define h (bound-h b))
  (define x (posn-x (bound-psn b)))
  (define y (posn-y (bound-psn b)))
  
  (define top? (and (< y hmid) (< (+ y h) hmid)))
  (define bot? (> y hmid))

  (when (and (< x vmid) (< w vmid))
    (begin
      (when top? (set! index 0))
      (when bot? (set! index 2))))

  (when (> x vmid)
    (begin
      (when top? (set! index 1))
      (when bot? (set! index 3))))

  index)

(define (insert! qt b)
  (define nodes (quadtree-nodes qt))
  (define done? #f)
  (when (not (eq? (vector-ref nodes 0) #f))
    (define idx (get-index b))
    (when (not (eq? idx -1))
      (vector-set! nodes idx b)
      (set! done? #t)))
  (when (not done?)
    (begin
      (set-quadtree-objects! qt (append b (quadtree-objects qt)))
      (when (and
             (> (length (quadtree-objects qt)) MAXOBJECTS)
             (< (quadtree-level qt) MAXLEVELS))
        (begin
          (when (eq? (vector-ref nodes 0) #f)
            (split! qt))
          (define rem (list))
          (for ([i (in-range (length (quadtree-objects qt)))])
                (begin
                  (define index (get-index (list-ref i (quadtree-objects qt))))
                  (if (not (eq? index -1))
                      (insert!
                       (vector-ref index (quadtree-nodes qt))
                       (list-ref i (quadtree-objects qt)))
                      (set! rem (append (list-ref i (quadtree-objects qt)) rem)))))
          (set-quadtree-objects! qt rem))))))

(define (retrieve qt b)
  (define index (get-index qt b))
  (define nodes (quadtree-nodes qt))
  (define robjs (quadtree-objects qt))
  (when (eq? (vector-ref nodes 0) #f)
    (if (not (eq? index -1))
        (set! robjs (append robjs (retrieve (vector-ref index nodes) b)))
        (for ([i (in-range (length nodes))])
          (set! robjs (append robjs (retrieve (vector-ref i nodes) b))))))
  robjs)

        



  

  
  
  

  