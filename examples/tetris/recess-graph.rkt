#lang racket/base
(require (for-syntax syntax/parse)
         (for-syntax racket/base)
         (for-syntax racket/list)
         graph)

(provide
 (all-defined-out)
 (all-from-out racket/base)
 (all-from-out graph))

(define recess-graph (unweighted-graph/directed '()))

;; A component is an identifier [and an expression]
;; Seems that components should be lambdas eventually
;; to support initialization
(define-syntax (define-component stx)
  (syntax-parse stx
    [(_ name [~optional body])
     (begin
       #;#''(name (~? body '()))
       #'(define name (gensym)))]))

;; list of components
(define-syntax (define-archetype stx)
  (syntax-parse stx
    [(_ (name [arg-id default-expr] ...) components ...)
     #'(define name
         (lambda ([arg-id default-expr] ...)
           components ...))]))

;; An event is an identifier [also optionally a type predicate]

(define (create-event ident [pred #f])
  (struct event (ident pred) #:mutable)
  (event ident pred))

(define-syntax (define-event stx)
  (syntax-parse stx
    [(_ name (~optional pred) (~optional body))
     (begin
       #;#''(name (~? pred '()) (~? body '()))
       #'(define name (create-event 'name)))]))

;; register a system in the graph and have it print out the graph's
;; structure on each call
;; last graph printed is the full picture
(define-syntax (define-system stx)
  (syntax-parse stx
    [(_ system-name
        (~optional (~seq #:archetype archetype-name))
        (~seq #:on new-inputs)
        (~optional (~seq #:out new-outputs))
        (~optional (~seq #:map map-fn)))
     #'(begin
         (if-defined
          system-name
          system-name
          (define system-name 'system-name))
         (add-vertex! recess-graph 'system-name)
         (for-each
          (lambda (v)
            (begin
              (displayln v)
              (add-vertex! recess-graph v)
              (add-directed-edge! recess-graph v 'system-name)))
          new-inputs)
         (display (graphviz recess-graph)))]))

(define-syntax (events stx)
  (syntax-parse stx
    [(_ ev ...)
     #'(begin
         (cond
           [(identifier-binding #'ev) (define ev (gensym)) (list 'ev ...)] ...))]))

(define-syntax (if-defined stx)
  (syntax-case stx ()
    [(_ id iftrue iffalse)
     (let ([where (identifier-binding #'id)])
       (if where #'iftrue #'iffalse))]))
