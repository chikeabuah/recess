#lang racket/base

(require
  (for-syntax syntax/parse racket/base racket/syntax racket/match)
  graph racket/syntax racket/match racket/generic racket/contract racket/list)

(require (except-in racket set!)
         (rename-in racket [set! former-set!]))

(define (set! id expr [ref (λ (x) #f)])
  (cond [(entity? id) (set-entity! id expr ref)]
        [else (former-set! id expr)]))

(provide
 (all-defined-out)
 (all-from-out racket/base racket/syntax racket/match racket/list graph))

;; A component is an identifier
;; and optionally some other data

(struct component (id proto))

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
(define/contract (make-entity! cmpnts)
  (->  (listof component?) entity?)
  (let* ([e (create-entity (gensym))]
         [hash (make-hasheq)]
         [_ (for/list
                ([cmpnt cmpnts])
              ;; check if it's a component with data or not
              (if (component-proto cmpnt)
                  ;; NOTE: this means components are unique in an entity
                  (hash-set!
                   hash
                   (component-id cmpnt)
                   (component-proto cmpnt))
                  (hash-set!
                   hash
                   (component-id cmpnt)
                   #t)))]
         [_ (set-entity-components! e hash)])
    ;; add e to world
    (when (current-world) (add-entity-to-world! e (current-world)))
    e))

(define (create-entity id [cmpnts (λ (x) #t)])  
  (entity id cmpnts))

(define (set-entity! e expr [ref (λ (x) #f)])
  ;; if the entity has just one component then it is unambiguous
  ;; if it has more than one component then we need a reference to 
  ;; the component to set it
  (define cmpnts-hash (entity-components e))
  (define keys (hash-keys cmpnts-hash))
  (define cmpnts-length (length keys))
  (cond 
    [(eq? cmpnts-length 1) (hash-set! cmpnts-hash (first keys) expr)]
    [ref (hash-set! cmpnts-hash ref expr)]
    [else (raise "attempt to set entity was too ambiguous")]))

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

(define add-entity! make-entity!)

(define (get ent ref)
  (hash-ref (entity-components ent) ref))

;; worlds

;; entities are a make-hasheq
(struct world (name entities dependency-graph) #:mutable)

;; assuming that most programs will use a single world
;; we can support a single-world mode where things are
;; automatically added to the default world

;; if we ever need to keep track of a list of worlds
(struct universe (worlds) #:mutable)
(define recess-universe (universe '()))
(define current-world (make-parameter #f))
(define current-events (make-parameter (make-hasheq)))
(define start-time (make-parameter (current-seconds)))

;; create a topological ordering of the recess
;; graph and execute the nodes in that order
(define-syntax (begin-recess stx)
  (syntax-parse stx
    [(_ (~seq #:systems system-name:id ...)
        (~seq #:initialize init-expr:expr ...)
        (~seq #:stop-when stop-expr:expr ...))
     #'(parameterize ([current-world (world (gensym) (make-hasheq) (unweighted-graph/directed '()))]
                      [start-time (current-seconds)])
         (begin
           init-expr ...
           (define systems (list system-name ...))
           (for-each
            (λ (sys)
              (add-to-graph sys (system-in sys) (list) (world-dependency-graph (current-world))))
            systems)          
           (let loop ()
             ;; poll events
             (parameterize ([current-events (poll-events (current-events))])
               (displayln "executing recess graph...")
               (step-world))
             (when (systems-enabled? (list stop-expr ...)) (loop)))))]))

;; do a single iteration of a world
(define (step-world)
  (define tsorted-world (tsort (world-dependency-graph (current-world))))
  (for-each (λ (arg)
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
            tsorted-world))

;; the idea here is to poll the events by examing the hash values
;; if the hash value is a thunk we invoke it and replace the
;; thunk with its result
(define (poll-events evnts)
  (->  hash-eq? hash-eq?)
  (define (poll event-pair)
    (if (procedure? (cdr event-pair))
        (cons (car event-pair) ((cdr event-pair)))
        event-pair))
  (make-hasheq (map poll (hash->list evnts))))

(define (systems-enabled? systems)
  (let ([enabled? (λ (sys) (system-enabled sys))])
    (andmap enabled? systems)))

;; An event is an identifier [also optionally a type predicate]

;; event ideas:
;; requiring a system as an implicit event = requiring all of that system's output events
;; in addition to an implicit event matching the system's name

(define-generics event-generic
  [event-generic-name event-generic])

(struct event (name zero plus)
  #:methods gen:event-generic
  [(define (event-generic-name event-generic)
     (event-name event-generic))])
(struct event:source event (input))
(struct event:sink event (output))
(struct event:transform event (f))

(define (create-event name [value (λ (x) #t)] [zero (λ (x) #t)] [plus (λ (x) #t)])  
  (event name zero plus))

(define (set-event! key value)
  (hash-set! (current-events) key value))

(define-syntax (define-event stx)
  (syntax-parse stx
    [(_ name (~optional value) (~optional zero) (~optional plus))
     #'(begin
         ;; check first
         (define name (event 'name (~? zero (λ (x) #t)) (~? plus (λ (x) #t))))
         (hash-set! (current-events) name (~? value #f))
         name)]))

;;; i'm imagining a library of pre-defined source events
;;; the source events can take an input which is a lambda
;;; we want to poll the sources to produce their value

;; clock event

;; just an epoch for now
(define clock/e (event:source 'clock/e #f #f (λ () (- (current-seconds) (start-time)))))
;; record value in the hash table as a lambda
(hash-set! (current-events) 'clock/e (λ () (- (current-seconds) (start-time))))

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
  #:methods gen:event-generic
  [(define (event-generic-name event-generic)
     (system-id event-generic))]
  #:mutable)

(define (create-system
         name
         [body #f]
         [in #f]
         [state #f]
         [pre #f]
         [enabled #f]
         [query #f]
         [map  #f]
         [reduce #f]
         [post  #f]
         [out #f])
  (system name body in state pre enabled query map reduce post out))

;; register a system in the graph and have it print out the graph's
;; structure on each call
;; last graph printed is the full picture

(define-syntax (define-system stx)
  (syntax-parse stx
    [(_ system-name:id
        (~seq #:in [evt-name:id evt:expr]) ...
        (~optional (~seq #:state [given-state-name:id initial-state:expr]))
        (~optional (~seq #:pre given-pre-name:id pre-body:expr ...))
        (~optional (~seq #:enabled? enabled?-body:expr ...))
        (~optional (~seq #:query given-entity-name:id query:expr #;query:static-query))
        (~optional (~seq #:map given-maps-name:id map-body:expr ...))
        (~optional (~seq #:reduce given-reduce-name:id zero-expr:expr reduce-body:expr ...))
        (~optional (~seq #:post post-body:expr ...))
        (~seq #:out [out-evt:expr evt-val-body:expr ...]) ...)
     #:with state-name #'(~? given-state-name default-state-name)
     #:with pre-name #'(~? given-pre-name default-pre-name)
     #:with maps-name #'(~? given-maps-name default-maps-name)
     #:with reduce-name #'(~? given-reduce-name default-reduce-name)
     #:with entities-name #'(~? given-entity-name default-entity-name)
     #'(begin   
         (define system-name (create-system 'system-name))
         (set-system-in! system-name (list evt ...))
         (hash-set! (current-events) 'system-name  #f)
         (set-system-body!
          system-name
          (λ (sys)
            (let*
                ([prior-state (system-state sys)]
                 [pre-body-fun (λ (state-name evts)
                                 (display "evts")(display evts)
                                 (match-define (list evt-name ...) evts)
                                 (~? (begin pre-body ...) (void)))]
                 [enabled-body-fun (λ (state-name pre-name evts)
                                     (match-define (list evt-name ...) evts)
                                     (~? (and enabled?-body ...) (void)))]         
                 [post-body-fun (λ (state-name pre-name reduce-name)
                                  (~? (begin post-body ...) (void)))]
                 [output-events-fun
                  (λ (state-name pre-name reduce-name)
                    (create-event 'out-event (~? (begin evt-val-body ...) void) ...))]
                 [state-0 (if prior-state prior-state (~? initial-state #f))]
                 [state-name state-0]
                 [get-event-vals (λ (ev) (hash-ref (current-events) (event-generic-name ev)))]
                 [event-vals (map get-event-vals (filter event-generic? (list evt ...)))]
                 [pre-val-0 (pre-body-fun state-name event-vals)]
                 [pre-name pre-val-0]
                 [state-name (if (not (void? pre-name)) pre-name state-name)]
                 [input-events (let-values ([(evt-name ...) (values evt ...)])
                                 (list evt-name ...))]
                 [enabled (enabled-body-fun state-name pre-name event-vals)]
                 [entities
                  (if enabled (~? query (list)) (list))]
                 [map-body-fun (λ (state-name pre-name entities-name)
                                 (~? (map (λ (entities-name) map-body ...) entities) (void)))]
                 [reduce-body-fun (λ (state-name pre-name maps-name)
                                    (~? (begin (foldl reduce-body zero-expr entities) ...) (void)))]
                 [maps-val
                  (if
                   enabled
                   (map-body-fun state-name pre-name entities)
                   (list))]
                 [maps-name maps-val]
                 [reduce-val (reduce-body-fun state-name pre-name maps-name)]
                 [reduce-name reduce-val]
                 [post (post-body-fun state-name pre-name reduce-name)]
                 [state-name (if (not (void? post)) post state-name)]
                 [output-events (output-events-fun state-name pre-name reduce-name)])
              (begin
                (set-system-state! system-name state-name)
                (set-system-enabled! system-name enabled)))))
         system-name)]))

;; helper methods

(define (all-defined-systems)
  'all-defined-systems)

(define (lookup archetype . rest)
  (define entities (world-entities (current-world)))
  (define (archetype-match? ent)
    (equal?
     (list->set (map component-id (cons archetype rest)))
     (list->set (map car (hash->list (entity-components ent))))))
  (define matches (filter archetype-match? (map cdr (hash->list entities))))
  #;(displayln matches)
  matches)


(define (add-to-graph system-name input-events output-events recess-graph)
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
