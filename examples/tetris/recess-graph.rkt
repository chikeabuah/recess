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

;;; define-system syntax and identifier bindings

#;(define-system system:id
  ;; a specification of the 'type' of entities that are needed
  #:archetype archetype:id
  ;; initial state of the system
  #:init initial-state:expr
  ;; Runs before the iteration to potentially update the state
  ;; and gather some data for this iteration
  ;; `system-state` is bound to the system's current state in pre-body
  ;; `in-events` is bound to the system's input events in pre-body
  #:pre pre-body:expr
  ;; generates the final state of the system for the iteration and then produces output
  ;; `system-state` is bound to the system's current state in post-body 
  #:post post-body:expr
  ;; decides if the query should be run (post will run anyway)
  ;; `system-state` is bound to the system's current state in pred-expr
  #:enabled pred-expr:expr
  ;; a static specification of which events are inputs
  #:on list-of-events:expr
  ;; a static specification of which events may be output
  #:out list-of-events:expr
  ;; processes a single entity
  ;; `system-state` is bound to the system's current state in map-body  
  ;; `e` is bound to the entity in map-body
  #:map map-body:expr
  ;; combines the results of many entities
  ;; `system-state` is bound to the system's current state in reduce-body
  ;; `e` is bound to the entity in reduce-body  
  #:reduce reduce-body:expr
  ;; the unit of the reduction for the iteration
  ;; `system-state` is bound to the system's current state in zero-expr
  #:zero zero-expr:expr)


;; register a system in the graph and have it print out the graph's
;; structure on each call
;; last graph printed is the full picture

(define-syntax (define-system stx)
  (syntax-parse stx
    [(_ system-name
        (~optional (~seq #:archetype archetype-name))
        (~seq #:on new-inputs-expr)
        (~optional (~seq #:out new-outputs))
        (~optional (~seq #:map map-fn)))
     #'(begin
         (unless-defined
          system-name
          (define system-name 'system-name))
         (add-vertex! recess-graph system-name)
         (for-each
          (lambda (v)
            (begin
              (add-vertex! recess-graph v)
              (add-directed-edge! recess-graph v system-name)))
          new-inputs-expr)
         (display (graphviz recess-graph)))]))

(define-syntax (events stx)
  (syntax-parse stx
    [(_ ev ...)
     #'(begin
         (cond
           [(and (identifier-binding #'ev) ...) (list 'ev ...)]))]))

(define-syntax (unless-defined stx)
  (syntax-case stx ()
    [(_ id then)
     (cond
       [(identifier-binding #'id) #'id]
       [else #'then])]))
