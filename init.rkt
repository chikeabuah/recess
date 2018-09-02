#lang racket/base

(require graph racket/match)

(provide
 (all-defined-out)
 (all-from-out graph))

(define (recess-init init-func system-in-out-name-lists recess-graph)
  ;; user's init expressions
  (init-func)
  ;; build up the dependency graph
  (for-each
   (λ (sys-in-out-name-list)
     (match-define (list in out name) sys-in-out-name-list)
     ((add-to-graph recess-graph) name in out))
   system-in-out-name-lists)
  (tsort recess-graph))

;; add a system to the dependency graph in the current world
(define (add-to-graph recess-graph)
  (λ (system-name input-events output-events)
    (begin
      (add-vertex! recess-graph system-name)
      (for-each
       (λ (ev)
         (begin
           (add-vertex! recess-graph ev)
           (add-directed-edge! recess-graph ev system-name)))
       input-events)
      (for-each
       (λ (ev)
         (begin
           (add-vertex! recess-graph ev)
           (add-directed-edge! recess-graph system-name ev)))
       output-events)
      #;(display (graphviz recess-graph)))))