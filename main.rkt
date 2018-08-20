#lang racket/base

(require
  (for-syntax syntax/parse racket/base racket/syntax racket/match)
  graph racket/syntax racket/match racket/generic racket/contract)

(require (except-in racket set!)
         (rename-in racket [set! former-set!]))

(define (set! id expr [ref (λ (x) #f)])
  (cond [(entity? id) (set-entity! id expr ref)]
        [else (former-set! id expr)]))

(provide
 (all-defined-out)
 (all-from-out racket/base racket/syntax racket/match graph))

;; A component is an identifier
;; and optionally a struct that implements the component-generic

;; The inner component generic describes the optional inner struct of the
;; component which contains data.
(define-generics component-prototype-generic
  [init-component component-prototype-generic])

(struct component (id proto))

(struct component:instance component (data) #:mutable)

(define-syntax (define-component stx)
  (syntax-parse stx
    [(_ name [~optional given-generic])
     (with-syntax ([gen:name (format-id #'name "gen:~a" (syntax-e #'name))])
       #'(begin
           ;; this is so we can search for a component type with
           ;; something like: Shape?, Timer?, etc
           (define-generics name)
           (struct component:generic component () #:methods gen:name [])
           (define (create-component id [generic (λ (x) #f)])  
             (component:generic id generic))
           (define name (create-component 'name (~? given-generic #f)))))]))

;; list of components
(define-syntax (define-archetype stx)
  (syntax-parse stx
    [(_ name components ...)
     #'(define name
         (list components ...))]))

;; entities

;; cmpnts is a (make-hasheq)
(struct entity (id components) #:mutable)

;; accepts a list of components
(define/contract (make-entity cmpnts)
  (->  (listof component?) entity?)
  (let* ([e (create-entity (gensym))]
         [hash (make-hasheq)]
         [_ (for/list
                ([cmpnt cmpnts])
              ;; check if it's a component with data or not
              (if (component-proto cmpnt)
                  ;; this means component types are unique in an entity
                  (hash-set!
                   hash
                   (component-id cmpnt)
                   (component:instance cmpnt (init-component cmpnt)))
                  (hash-set!
                   hash
                   (component-id cmpnt)
                   (component:instance cmpnt #f))))]
         [_ (set-entity-components! e hash)])
    ;; add e to world
    (when (current-world) (add-entity-to-world! e (current-world)))))

(define (create-entity id [cmpnts (λ (x) #t)])  
  (entity id cmpnts))

(define (set-entity! e expr [ref (λ (x) #f)])
  ;; if the entity has just one component then it is unambiguous
  ;; if it has more than one component then we need a reference to 
  ;; the component to set it
  (let* ([cmpnts-hash (entity-components e)]
         [keys (hash-keys cmpnts-hash)]
         [cmpnts-length (length keys)])
    (cond 
      [(eq? cmpnts-length 1) (hash-set! cmpnts-hash (first keys) expr)]
      [ref (hash-set! cmpnts-hash ref expr)]
      [else (raise "attempt to set entity was too ambiguous")])))

(define (add-entity-to-world! e wrld)
  (let ([current-entities (world-entities wrld)])
    (hash-set! current-entities (entity-id e) e)))

(define (remove-entity-from-world! e wrld)
  (let ([current-entities (world-entities wrld)])
    (hash-remove! current-entities (entity-id e))))

(define (get-entities-with-archetype wrld atype)
  1)

(define (entity-has-archetype? ent atype)
  2)

(define (entity-contains-archetype? ent atype)
  3)

(define (add-entity! e)
  'add-entity-to-world)

;; worlds

(define recess-graph (unweighted-graph/directed '()))

;; entities are a make-hasheq
(struct world (name entities dependency-graph) #:mutable)

;; assuming that most programs will use a single world
;; we can support a single-world mode where things are
;; automatically added to the default world

;; if we ever need to keep track of a list of worlds
(struct universe (worlds) #:mutable)
(define recess-universe (universe '()))
(define current-world (make-parameter #f))

;; create a topological ordering of the recess
;; graph and execute the nodes in that order
(define-syntax (begin-recess stx)
  (syntax-parse stx
    [(_ (~seq #:systems system-name:id ...)
        (~seq #:initialize init-expr:expr ...)
        (~seq #:stop-when stop-expr:expr ...))
     #'(parameterize ([current-world (world (gensym) (make-hasheq) recess-graph)])
         (let ([world-tsorted (tsort recess-graph)])
           (begin
             init-expr ...
             (let loop ()
               (displayln "executing recess graph...")
               (for-each (lambda (arg)
                           (cond
                             [(event? arg)
                              (display "this is an event:")
                              (display arg)
                              (displayln (event-name arg))]
                             [(system? arg)
                              (display "this is a system:")
                              (display arg)
                              (displayln (system-id arg))
                              (displayln "executing system:")
                              ((system-body arg) arg)]
                             [else (display "unknown") (displayln arg)]))
                         world-tsorted)
               (when (systems-enabled? (list stop-expr ...)) (loop))))))]))

(define (systems-enabled? systems)
  (let ([enabled? (λ (sys) (system-enabled sys))])
    (andmap enabled? systems)))

;; An event is an identifier [also optionally a type predicate]

;; event ideas:
;; requiring a system as an implicit event = requiring all of that system's output events
;; in addition to an implicit event matching the system's name

(define evts (make-hasheq))

(define-generics event-generic)

(struct event (name value zero plus) #:methods gen:event-generic [])
(struct event:source event (input))
(struct event:sink event (output))
(struct event:transform event (f))

(define (create-event id [value (λ (x) #t)] [zero (λ (x) #t)] [plus (λ (x) #t)])  
  (event id value zero plus))

(define (set-event! key value)
  (hash-set! evts key value))

(define-syntax (define-event stx)
  (syntax-parse stx
    [(_ name (~optional value) (~optional zero) (~optional plus))
     #'(begin
         ;; check first
         (define name (event 'name (~? value (λ (x) #t)) (~? zero (λ (x) #t)) (~? plus (λ (x) #t))))
         (hash-set! evts name (~? value #f))
         name)]))

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
  (id body in state pre enabled query map reduce post out)
  #:methods gen:system-generic []
  #:methods gen:event-generic []
  #:mutable)

(define (create-system
         name
         [body (λ (x) #f)]
         [in (λ (x) #f)]
         [state #f]
         [pre (λ (x) #f)]
         [enabled (λ (x) #f)]
         [query (λ (x) #f)]
         [map (λ (x) #f)]
         [reduce (λ (x) #f)]
         [post (λ (x) #f)]
         [out (λ (x) #f)])
  (system name body in state pre enabled query map reduce post out))

;; register a system in the graph and have it print out the graph's
;; structure on each call
;; last graph printed is the full picture

(define-syntax (define-system stx)
  (syntax-parse stx
    [(_ system-name:id
        (~seq #:in [in-evt-name:id in-evt-body:expr]) ...
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
          (λ (sys)
            (let*
                ([prior-state (system-state sys)]
                 [pre-body-fun (λ (state-name events)
                                 #;(match-define (list evt-name ...) (hash->list events))
                                 (~? (begin pre-body ...) (void)))]
                 [input-events-fun (λ (pre-name)
                                     (let ([input-events
                                            (list (create-event 'in-evt-name in-evt-body) ... )])
                                       input-events))]
                 [enabled-body-fun (λ (state-name pre-name)
                                     (~? (and enabled?-body ...) (void)))]
                 [map-body-fun (λ (state-name pre-name)
                                 (~? (begin map-body ...) (void)))]
                 [reduce-body-fun (λ (state-name pre-name maps-name)
                                    (~? (begin reduce-body ...) (void)))]
                 [post-body-fun (λ (state-name pre-name reduce-name)
                                  (~? (begin post-body ...) (void)))]
                 [output-events-fun
                  (λ (state-name pre-name reduce-name)
                    (create-event 'out-event (~? (begin evt-val-body ...) void) ...))]
                 [state-0 (if prior-state prior-state (~? initial-state #f)) ]
                 [state-name state-0]
                 [pre-val-0 (pre-body-fun state-name evts)]
                 [pre-name pre-val-0]
                 [state-name pre-name]
                 [input-events (input-events-fun pre-name)]
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
                 [output-events (output-events-fun state-name pre-name reduce-name)])
              (begin
                #;(map map-name entities)
                #;(foldl reduce-name zero entities)
                (set-system-in! system-name input-events)
                (set-system-state! system-name state-name)
                (set-system-enabled! system-name enabled)))))
         ((system-body system-name) system-name)
         (add-to-graph system-name (system-in system-name) (list))
         system-name)]))

;; helper methods

(define (all-defined-systems)
  'all-defined-systems)

(define (query archetype)
  ;; TODO: implement
  (list 1 2 3))

(define (add-to-graph system-name input-events output-events)
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
    #;(display (graphviz recess-graph))))
