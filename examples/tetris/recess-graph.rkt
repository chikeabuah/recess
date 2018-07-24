#lang racket/base
(require (for-syntax syntax/parse racket/base racket/list)
         graph)

(provide
 (all-defined-out)
 (all-from-out racket/base graph))

(define recess-graph (unweighted-graph/directed '()))

;; A component is an identifier [and an expression]
;; Seems that components should be lambdas eventually
;; to support initialization

(struct component (id type))

(define (create-component id [type (lambda (x) #t)])  
  (event id type))

(define-syntax (define-component stx)
  (syntax-parse stx
    [(_ name [~optional type])
     (begin
       #;#''(name (~? type #f))
       #'(define name (create-component 'name (~? type #f))))]))

;; list of components
(define-syntax (define-archetype stx)
  (syntax-parse stx
    [(_ (name [arg-id default-expr] ...) components ...)
     #'(define name
         (lambda ([arg-id default-expr] ...)
           components ...))]))

;; An event is an identifier [also optionally a type predicate]

(struct event (id type))

(define (create-event id [type (lambda (x) #t)])  
  (event id type))

(define-syntax (define-event stx)
  (syntax-parse stx
    [(_ name (~optional type))
     (begin
       #;#''(name (~? type #f))
       #'(define name (create-event 'name (~? 'type #f))))]))

;;; define-system syntax and identifier bindings

;;; let's list these in the order they should be bound/evaluated
;;; top->bottom

#;(define-system system-name:id
    ;; a specification of the 'type' of entities that are needed
    #:archetype archetype:id
    ;; a static specification of which events are inputs
    #:on list-of-input-events:expr
    ;; a static specification of which events may be output
    #:out list-of-output-events:expr
    ;; initial state of the system (struct?)
    #:init initial-state:expr
    ;; Runs before the iteration to potentially update the state
    ;; and gather some data for this iteration
    ;; `system-state` is bound to the system's current state (initial-state) in pre-body
    ;; `in-events` is bound to the system's list-of-input-events in pre-body
    ;; returns a `system-state` which has been altered from its initial-state
    ;; based on the 'in-events` to (system-state x B)
    #:pre pre-body:expr
    ;; decides if the query should be run (post will run anyway)
    ;; `system-state` is bound to the system's current state (system-state x B) in pred-expr
    ;; returns a boolean
    #:enabled pred-expr:expr
    ;; `system-state` is bound to the system's current state (system-state x B) in zero-expr
    ;; returns the unit of the reduction for the iteration of type A
    #:zero zero-expr:expr
    ;; processes a single entity
    ;; `system-state` is bound to the system's current state (system-state x B) in map-body  
    ;; `e` is bound to the entity from the query for this iteration in map-body
    ;; processes a single entity and returns a value of type A -- the type of the reduction
    #:map map-body:expr
    ;; combines the results of many entities (seq A)
    ;; `system-state` is bound to the system's current state (system-state x B) in reduce-body
    ;; `e` is bound to the entity in reduce-body
    ;; returns a value of type A 
    #:reduce reduce-body:expr
    ;; generates the final state of the system for the iteration and then produces output
    ;; `system-state` is bound to the system's current state (system-state x B x A) in post-body
    ;; returns a list-of-output-events and (system-state x C)
    #:post post-body:expr)

;; system struct
;; not sure yet if we will use
#;(struct system (system-name archetype on out init pre enabled zero map-body reduce-body post))

#;(define (create-system
         system-name
         [archetype (lambda (x) #t)]
         [on (lambda (x) #t)]
         [out (lambda (x) #t)]
         [init (lambda (x) #t)]
         [pre (lambda (x) #t)]
         [enabled (lambda (x) #t)]
         [zero (lambda (x) #t)]
         [map-body (lambda (x) #t)]
         [reduce-body (lambda (x) #t)]
         [post (lambda (x) #t)])
  (system system-name archetype on out init pre enabled zero map-body reduce-body post))


;; register a system in the graph and have it print out the graph's
;; structure on each call
;; last graph printed is the full picture

(define-syntax (define-system stx)
  (syntax-parse stx
    [(_ system-name
        (~optional (~seq #:archetype archetype-name))
        (~optional (~seq #:on new-inputs-expr))
        (~optional (~seq #:out new-outputs-expr))
        (~optional (~seq #:init init-expr))
        (~optional (~seq #:pre pre-body))
        (~optional (~seq #:enabled enabled-expr))
        (~optional (~seq #:zero zero-expr))
        (~optional (~seq #:map map-fn))
        (~optional (~seq #:reduce-body reduce-body-expr))
        (~optional (~seq #:post post-body)))
     #'(define system-name
         (let*
             ([archetype (~? archetype-name #f)]
              [input-events (map create-event new-inputs-expr)]
              [output-events (map create-event (~? new-outputs-expr (list)))]
              [system-state (~? init-expr #f)]
              [system-state-b (~? pre-body #f)]
              [is-enabled (~? enabled-expr #f)]
              [zero (~? zero-expr #f)]
              [map-body (~? 'map-fn #f)]
              [reduce-body (~? reduce-body-expr #f)]
              [post (~? post-body #f)])
           (begin
             #;(displayln input-events)
             (add-vertex! recess-graph 'system-name)
             (for-each
              (lambda (v)
                (begin
                  (add-vertex! recess-graph (event-id v))
                  (add-directed-edge! recess-graph (event-id v) 'system-name)))
              (~? input-events))
             (display (graphviz recess-graph)))))]))
         

(define-syntax (events stx)
  (syntax-parse stx
    [(_ ev ...)
     #'(begin
         (cond
           [(and (identifier-binding #'ev) ...) (list 'ev ...)]))]))
