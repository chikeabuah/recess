#lang racket
(require (for-syntax syntax/parse))
(require graph)

(provide
 (all-defined-out)
 (all-from-out racket))

(define recess-graph (unweighted-graph/directed '(a)))
(define-vertex-property recess-graph archetype)
(define-vertex-property recess-graph map-fn)
(define-vertex-property recess-graph parents #:init '())
(define-vertex-property recess-graph children #:init '())
(define-vertex-property recess-graph inputs #:init '())
(define-vertex-property recess-graph outputs #:init '())

;; A component is an identifier [and an expression]
;; Seems that components may be lambdas 
(define-syntax (define-component stx)
  (syntax-parse stx
   [(_ name [~optional body])
    #''(name (~? body '()))]))


(define-syntax (define-archetype stx)
  (syntax-parse stx
   [(_ (name [arg-id default-expr] ...) components ...)
    #''(define name
         (lambda ([arg-id default-expr] ...)
           (components ...)))]))

;; An event is an identifier [a predicate and an expression]
(define-syntax (define-event stx)
  (syntax-parse stx
   [(_ name [~optional pred] [~optional body])
    #''(name (~? pred '()) (~? body '()))]))


(define define-system
  (lambda
      (system-name
       #:archetype [archetype-name #f]
       #:on [new-inputs #f]
       #:out [new-outputs #f]
       #:depends [new-dependencies #f]
       #:map [map-fn #f])
    (begin
      (add-vertex! recess-graph system-name)
      (and archetype-name (archetype-set! system-name archetype-name))
      (and map-fn (map-fn-set! system-name map-fn))
      (and
       new-inputs
       (inputs-set! system-name (cons new-inputs (inputs system-name #:default '()))))
      (and
       new-outputs
       (outputs-set! system-name (cons new-outputs (outputs system-name #:default '()))))
      (and
       new-dependencies
       (parents-set! system-name (cons new-dependencies (parents system-name #:default '()))))
      (graphviz recess-graph))))


