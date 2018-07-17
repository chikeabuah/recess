#lang racket/base
(require (for-syntax syntax/parse))
(require (for-syntax racket/base))
(require graph)

(provide
 (all-defined-out)
 (all-from-out racket/base)
 (all-from-out graph))

(define recess-graph (weighted-graph/directed '()))

;; A component is an identifier [and an expression]
;; Seems that components should be lambdas eventually
;; to support initialization
(define-syntax (define-component stx)
  (syntax-parse stx
    [(_ name [~optional body])
     (begin
       #''(name (~? body '()))
       #'(define name (gensym))
       )
     ]))

;; list of components
(define-syntax (define-archetype stx)
  (syntax-parse stx
    [(_ (name [arg-id default-expr] ...) components ...)
     #'(define name
         (lambda ([arg-id default-expr] ...)
           components ...))]))

;; An event is an identifier [also optionally a predicate and an expression]
(define-syntax (define-event stx)
  (syntax-parse stx
    [(_ name [~optional pred] [~optional body])
     (begin
       #''(name (~? pred '()) (~? body '()))
       #'(define name (gensym))
       )
     ]))

;; register a system in the graph and have it print out the graph's
;; structure on each call
;; last graph printed is the full picture
(define-syntax (define-system stx)
  (syntax-parse stx
    [(_ system-name (~optional (~seq #:archetype archetype-name))
        (~optional (~seq #:on new-inputs))
        (~optional (~seq #:out new-outputs))
        (~optional (~seq #:depends new-dependencies))
        (~optional (~seq #:map map-fn)))
     #'(begin
         (add-vertex! recess-graph 'system-name)
         (for-each (lambda (parent)
                     (begin
                       (add-vertex! recess-graph 'system-name)
                       (add-directed-edge! recess-graph parent 'system-name (~? new-inputs ""))
                       )
                     )
                   (~? new-dependencies '()))
         (display (graphviz recess-graph)))
     ]))

