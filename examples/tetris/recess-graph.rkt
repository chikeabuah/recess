#lang racket/base
(require (for-syntax syntax/parse racket/base racket/syntax racket/match)
         graph racket/syntax racket/match racket/generic)

(provide
 (all-defined-out)
 (all-from-out racket/base racket/syntax racket/match graph))

(define recess-graph (unweighted-graph/directed '()))

;; A component is an identifier
;; and optionally a struct that implements the component-generic

;; The component generic describes the optional inner struct of the
;; component which contains data.
(define-generics component-generic
  [init-component component-generic]
  [randomize-component component-generic])

(define-syntax (define-component stx)
  (syntax-parse stx
    [(_ name [~optional generic])
     #'(begin
         ;; this is so we can search for a component type with
         ;; something like: Shape?, Timer?, etc
         (define-generics name)
         (struct component (id generic) #:methods gen:name [])
         (define (create-component id [generic (λ (x) #t)])  
           (component id generic))
         (define name (create-component 'name (~? generic #f))))]))

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

(define evts (make-hasheq))

(define-generics event-generic)

(struct event (name zero plus) #:methods gen:event-generic [])
(struct event:source event (input))
(struct event:sink event (output))
(struct event:transform event (f))

(define (create-event id [zero (λ (x) #t)] [plus (λ (x) #t)])  
  (event id zero plus))

(define-syntax (define-event stx)
  (syntax-parse stx
    [(_ name (~optional zero) (~optional plus))
     #'(begin
         ;; check first
         (define name (create-event 'name (~? 'zero #f) (~? 'plus #f)))
         (hash-set! evts name #f))]))

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

(define-generics system-generic)

(struct system
  (name body in state pre enabled query map reduce post out)
  #:methods gen:system-generic []
  #:methods gen:event-generic []
  #:mutable)

(define (create-system
         name
         [body (λ (x) #t)]
         [in (λ (x) #t)]
         [state (λ (x) #t)]
         [pre (λ (x) #t)]
         [enabled (λ (x) #t)]
         [query (λ (x) #t)]
         [map (λ (x) #t)]
         [reduce (λ (x) #t)]
         [post (λ (x) #t)]
         [out (λ (x) #t)])
  (system name body in state pre enabled query map reduce post out))

;; register a system in the graph and have it print out the graph's
;; structure on each call
;; last graph printed is the full picture

(define-syntax (define-system stx)
  (syntax-parse stx
    [(_ system-name:id
        (~seq #:in [evt-name:id in-evt:expr]) ...
        (~optional (~seq #:state [given-state-name:id initial-state:expr]))
        (~optional (~seq #:pre given-pre-name:id pre-body:expr ...))
        (~optional (~seq #:enabled? enabled?-body:expr ...))
        (~optional (~seq #:query entity-name:id query:expr #;query:static-query))
        (~optional (~seq #:map given-maps-name:id map-body:expr ...))
        (~optional (~seq #:reduce given-reduce-name:id reduce-body:expr ...))
        (~optional (~seq #:post post-body:expr ...))
        (~seq #:out [out-evt:expr evt-val-body:expr ...]) ...)
     #:with state-name #'(~? given-state-name default-state-name)
     #:with pre-name #'(~? given-pre-name default-pre-name)
     #:with maps-name #'(~? given-maps-name default-maps-name)
     #:with reduce-name #'(~? given-reduce-name default-reduce-name)
     #'(begin
           
         (define system-name (create-system 'system-name))
         (set-system-body!
          system-name
          (let*
              ([pre-body-fun (λ (state-name evts)
                               #;(match-define (list evt-name ...) (hash->list evts))
                               (~? (begin pre-body ...) (void)))]
               [enabled-body-fun (λ (state-name pre-name)
                                   (~? (begin post-body ...) (void)))]
               [map-body-fun (λ (state-name pre-name)
                               '(~? (begin map-body ...) (void)))]
               [reduce-body-fun (λ (state-name pre-name maps-name)
                                  (~? (begin reduce-body ...) (void)))]
               [post-body-fun (λ (state-name pre-name reduce-name)
                                (~? (begin post-body ...) (void)))]
               [output-events-fun (λ (state-name pre-name reduce-name)
                                    (~? (events out-evt ...) (void)))]
               [input-events (events evt-name ...)]
               [state-0 (~? initial-state #f)]
               [state-name state-0]
               [pre-val-0 (pre-body-fun state-name evts)]
               [pre-name pre-val-0]
               [enabled (enabled-body-fun state-name pre-name)]
               [entities
                (if enabled (~? query (list)) (list))]
               [maps-val
                (if
                 enabled
                 (map-body-fun state-name pre-name)
                 (list))]
               [maps-name maps-val]
               [reduce-val (reduce-body-fun state-name pre-name maps-name)]
               [reduce-name reduce-val]
               [post (post-body-fun state-name pre-name reduce-name)]
               [output-events (output-events-fun state-name pre-name reduce-name)]
               )
            (begin
              #;'(map map-body entities)
              #;(foldl reduce-body zero entities)
              (set-system-in! system-name input-events)
              (displayln state-name)
              #;(displayln output-events)
              #;(add-to-graph 'system-name input-events output-events)
              )))
         (add-to-graph system-name (system-in system-name) (list))
         system-name)]))


;; worlds

(define-syntax (define-world stx)
  (syntax-parse stx
    [(_ world-name:id action!:id)
     #'(action!)]))

;; create a topological ordering of the recess
;; graph and execute the nodes in that order
(define (start!)
  (let ([world-tsorted (tsort recess-graph)])
    (for-each (lambda (arg)
                (displayln arg))
              world-tsorted)))

;; entities

(struct entity (id cmpnts archetype) #:mutable)

(define (create-entity id [cmpnts (λ (x) #t)] [archetype (λ (x) #t)])  
  (entity id cmpnts archetype))

;; helper methods

(define-for-syntax (generate-temporary)
  (gensym))

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


;; helper macros

(define-syntax (events stx)
  (syntax-parse stx
    [(_ ev ...)
     #'(map create-event (list 'ev ...))]))
