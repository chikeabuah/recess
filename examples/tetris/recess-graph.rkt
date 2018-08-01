#lang racket/base
(require (for-syntax syntax/parse racket/base racket/syntax)
         graph racket/stxparam racket/syntax)

(provide
 (all-defined-out)
 (all-from-out racket/base racket/syntax graph))

(define recess-graph (unweighted-graph/directed '()))

;; A component is an identifier [and an expression]
;; Seems that components should be λs eventually
;; to support initialization

(struct component (id type))

(define (create-component id [type (λ (x) #t)])  
  (component id type))

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

(struct event (name zero plus))
(struct evnt:source event (input))
(struct event:sink event (output))
(struct event:transform event (f))

(define (create-event id [zero (λ (x) #t)] [plus (λ (x) #t)])  
  (event id zero plus))

(define-syntax (define-event stx)
  (syntax-parse stx
    [(_ name (~optional zero) (~optional plus))
     (begin
       #'(define name (create-event 'name (~? 'zero #f) (~? 'plus #f))))]))

;;; define-system syntax and identifier bindings

;;; let's list these in the order they should be bound/evaluated
;;; top->bottom

;; binds name as a system object
;; binds name as an event object
#;(define-system name:id
    ;; evt is evaluated once upon recess initialization [perhaps this
    ;; should be a special form of "event expression?" same with below.]
    (~seq #:in [evt-name:id evt:expr]) ...
    ;; initial-state is evaluated once upon recess initialization and
    ;; becomes state-0
    ;; default state-name is (generate-temporary)
    ;; default initial-state is #f
    (~optional (~seq #:state [state-name:id initial-state:expr]))
    ;; default pre-name is (generate-temporary)
    ;; state-name is bound inside pre-body to state-0
    ;; evt-name ... is bound to evt value
    ;; may modify state (changed version is state-1)
    ;; returns pre-val-0
    ;; default is (void)
    (~optional (~seq #:pre pre-name:id pre-body:expr ...))
    ;; state-name is bound inside enabled?-body to state-1
    ;; pre-name is bound to pre-val-0
    ;; default is #t
    ;; if #f, then query is not run, map is not run and
    ;; reduce is run with empty sequence.
    (~optional (~seq #:enabled? enabled?-body:expr ...))
    ;; query is a static-query
    ;; static-query :=
    ;;   [component-name:id component-binding:static-component]
    ;;   ...
    ;; static-component :=
    ;;   component-binding:id (bound to a component)
    ;;   archetype-binding:id (bound to an archetype)
    ;; entity-name defaults to (generate-temporary)
    (~optional (~seq #:query entity-name:id query:static-query))
    ;; state-name is bound inside map-body to state-1
    ;; pre-name is bound to pre-val-0
    ;; entity-name is bound to the entity
    ;; maps-name defaults to (generate-temporary)
    ;; returns map-val for entity (type A)
    (~optional (~seq #:map maps-name:id map-body:expr ...))
    ;; state-name is bound inside reduce-body to state-1
    ;; pre-name is bound to pre-val-0
    ;; maps-name is bound to a sequence of the map-values (could be empty)
    ;; returns reduce-val (type A)
    ;; default is (void)
    ;; reduce-name defaults to (generate-temporary)
    (~optional (~seq #:reduce reduce-name:id reduce-body:expr ...))
    ;; state-name is bound inside post-body to state-1
    ;; pre-name is bound to pre-val-0
    ;; reduce-name is bound to complete reduce result
    ;; returns state-N which becomes the new state-0 next iteration
    (~optional (~seq #:post post-body:expr ...))
    ;; state-name is bound inside evt-val-body to state-N
    ;; pre-name is bound to pre-val-0
    ;; reduce-name is bound to complete reduce result
    ;; evt should evaluate to an event (and is evaluated upon recess
    ;; initialization)
    ;; evt-val-body should evaluate to value of the evt type
    (~seq #:out [evt:expr evt-val-body:expr ...]) ...)

;; system struct
(struct system (name in state pre enabled query map reduce post out))

(define (create-system
         name
         [in (λ (x) #t)]
         [state (λ (x) #t)]
         [pre (λ (x) #t)]
         [enabled (λ (x) #t)]
         [query (λ (x) #t)]
         [map (λ (x) #t)]
         [reduce (λ (x) #t)]
         [post (λ (x) #t)]
         [out (λ (x) #t)])
  (system name in state pre enabled query map reduce post out))

;; register a system in the graph and have it print out the graph's
;; structure on each call
;; last graph printed is the full picture

(define-syntax (define-system stx)
  (syntax-parse stx
    [(_ system-name:id
        (~seq #:in [evt-name:id in-evt:expr]) ...
        (~optional (~seq #:state [state-name:id initial-state:expr]))
        (~optional (~seq #:pre pre-name:id pre-body:expr ...))
        (~optional (~seq #:enabled? enabled?-body:expr ...))
        (~optional (~seq #:query entity-name:id query:expr #;query:static-query))
        (~optional (~seq #:map maps-name:id map-body:expr ...))
        (~optional (~seq #:reduce reduce-name:id reduce-body:expr ...))
        (~optional (~seq #:post post-body:expr ...))
        (~seq #:out [out-evt:expr evt-val-body:expr ...]) ...)
     (with-syntax ([implicit-system-event (format-id #'system-name "~a/e" (syntax-e #'system-name))])
       #'(begin
           (define-event implicit-system-event)
           ;;(define)
           (let*
               ([input-events (events evt-name ...)]
                [output-events (events out-evt ...)]
                [system-state (~? initial-state (λ (x) #t))]
                ;              [system-state-b (syntax-parameterize
                ;                                  ([state (make-rename-transformer #'system-state)])
                ;                                (~? pre-body (λ (x) #t)))]
                ;              [is-enabled (syntax-parameterize
                ;                              ([state (make-rename-transformer #'system-state-b)])
                ;                            (~? enabled-expr (λ (x) #t)))]
                ;              [entities (query archetype)]
                ;              [zero (syntax-parameterize
                ;                        ([state (make-rename-transformer #'system-state-b)])
                ;                      (~? zero-expr (λ (x) #t)))]
                ;              [map-body (syntax-parameterize
                ;                            ([state (make-rename-transformer #'system-state)])
                ;                          (~? 'map-fn (λ (x) #t)))]
                ;              [reduce-body (syntax-parameterize
                ;                               ([state (make-rename-transformer #'system-state)])
                ;                             (~? reduce-body-expr (λ (x y) #t)))]
                ;              [post (syntax-parameterize
                ;                        ([state (make-rename-transformer #'system-state)])
                ;                      (~? post-body-expr (λ (x) #t)))]
                )
             (begin
               (displayln 'x)
               #;'(map map-body entities)
               #;(foldl reduce-body zero entities)
               1
               #;(add-to-graph 'system-name input-events output-events)))))]))


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
         (add-vertex! recess-graph (event-name ev))
         (add-directed-edge! recess-graph (event-name ev) system-name)))
     input-events)
    (for-each
     (λ (ev)
       (begin
         (add-vertex! recess-graph (event-name ev))
         (add-directed-edge! recess-graph system-name (event-name ev))))
     output-events)
    #;(display (graphviz recess-graph))))

;; syntax parameters

(define-syntax-parameter state
  (λ (stx)
    (raise-syntax-error (syntax-e stx) "can only be used inside define-system")))

;; helper macros

(define-syntax (events stx)
  (syntax-parse stx
    [(_ ev ...)
     #'(map create-event (list 'ev ...))]))
