#lang racket/base
(require (for-syntax syntax/parse racket/base)
         graph racket/stxparam)

(provide
 (all-defined-out)
 (all-from-out racket/base graph))

(define recess-graph (unweighted-graph/directed '()))

;; A component is an identifier [and an expression]
;; Seems that components should be λs eventually
;; to support initialization

(struct component (id type))

(define (create-component id [type (λ (x) #t)])  
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
         (λ ([arg-id default-expr] ...)
           components ...))]))

;; An event is an identifier [also optionally a type predicate]

;; event ideas:
;; requiring a system as an implicit event = requiring all of that system's output events
;; in addition to an implicit event matching the system's name 

(struct event (id type))

(define (create-event id [type (λ (x) #t)])  
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
    ;; initial state of the system
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
           [archetype (λ (x) #t)]
           [on (λ (x) #t)]
           [out (λ (x) #t)]
           [init (λ (x) #t)]
           [pre (λ (x) #t)]
           [enabled (λ (x) #t)]
           [zero (λ (x) #t)]
           [map-body (λ (x) #t)]
           [reduce-body (λ (x) #t)]
           [post (λ (x) #t)])
    (system system-name archetype on out init pre enabled zero map-body reduce-body post))


;; register a system in the graph and have it print out the graph's
;; structure on each call
;; last graph printed is the full picture

(define-syntax (define-system stx)
  (syntax-parse stx
    [(_ system-name:id
        (~alt
         (~optional (~seq #:archetype archetype-name:id))
         (~optional (~seq #:on new-inputs-expr:expr))
         (~optional (~seq #:out new-outputs-expr:expr))
         (~optional (~seq #:init init-expr:expr))
         (~optional (~seq #:pre pre-body:expr))
         (~optional (~seq #:enabled enabled-expr:expr))
         (~optional (~seq #:zero zero-expr:expr))
         (~optional (~seq #:map map-fn:expr))
         (~optional (~seq #:reduce reduce-body-expr:expr))
         (~optional (~seq #:post post-body-expr:expr))) ...)
     #'(define system-name
         (let*
             ([archetype (~? archetype-name (λ (x) #t))]
              [input-events (~? new-inputs-expr null)]
              [output-events (~? new-outputs-expr null)]
              [system-state (~? init-expr (λ (x) #t))]
              [system-state-b (syntax-parameterize
                                  ([state (make-rename-transformer #'system-state)])
                                (~? pre-body (λ (x) #t)))]
              [is-enabled (syntax-parameterize
                              ([state (make-rename-transformer #'system-state-b)])
                            (~? enabled-expr (λ (x) #t)))]
              [entities (query archetype)]
              [zero (syntax-parameterize
                        ([state (make-rename-transformer #'system-state-b)])
                      (~? zero-expr (λ (x) #t)))]
              [map-body (syntax-parameterize
                            ([state (make-rename-transformer #'system-state)])
                          (~? 'map-fn (λ (x) #t)))]
              [reduce-body (syntax-parameterize
                               ([state (make-rename-transformer #'system-state)])
                             (~? reduce-body-expr (λ (x y) #t)))]
              [post (syntax-parameterize
                        ([state (make-rename-transformer #'system-state)])
                      (~? post-body-expr (λ (x) #t)))])
           (begin
             #;(displayln system-state-b)
             '(map map-body entities)
             (foldl reduce-body zero entities)
             (post 1)
             (add-to-graph 'system-name input-events output-events))))]))


;; entities

(struct entity (id cmpnts archetype) #:mutable)

(define (create-entity id [cmpnts (λ (x) #t)] [archetype (λ (x) #t)])  
  (entity id cmpnts archetype))

;; helper methods

(define (query archetype)
  ;; TODO: implement
  (list 1 2 3))

(define (add-to-graph system-name input-events output-events)
  (begin
    (add-vertex! recess-graph system-name)
    (for-each
     (λ (ev)
       (begin
         (add-vertex! recess-graph (event-id ev))
         (add-directed-edge! recess-graph (event-id ev) system-name)))
     input-events)
    (for-each
     (λ (ev)
       (begin
         (add-vertex! recess-graph (event-id ev))
         (add-directed-edge! recess-graph system-name (event-id ev))))
     output-events)
    (display (graphviz recess-graph))))

;; syntax parameters

(define-syntax-parameter state
  (λ (stx)
    (raise-syntax-error (syntax-e stx) "can only be used inside define-system")))

;; helper macros

(define-syntax (events stx)
  (syntax-parse stx
    [(_ ev ...)
     #'(map create-event (list 'ev ...))]))
