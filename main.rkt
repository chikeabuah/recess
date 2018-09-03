#lang racket/base

(require
  (for-syntax
   syntax/parse
   racket/base
   racket/syntax
   racket/match)
  recess/init
  racket/syntax
  racket/match
  racket/generic
  racket/hash
  racket/contract
  racket/list)

(require (except-in racket set! + -)
         (rename-in racket
                    [set! former-set!]
                    [+ former-plus]
                    [- former-minus]))

;; can we replace this with a make-set!-transformer?
(define (set! id expr [ref (λ (x) #f)])
  (cond [(entity? id) (set-entity! id expr ref)]
        [else (former-set! id expr)]))

;; this is an attempt to simplify modifying entities
(define (+ ent . cmpnts)
  (cond [(entity? ent) (add-components-to-entity ent cmpnts)]
        [else (apply former-plus (cons ent cmpnts))]))

(define (- ent . cmpnts)
  (cond [(entity? ent) (remove-components-from-entity ent cmpnts)]
        [else (apply former-minus (cons ent cmpnts))]))

(provide
 (all-defined-out)
 (all-from-out racket/base racket/syntax racket/match racket/list))

;; A component is an identifier
;; and optionally some other data
;; currently still experimenting with the idea of having the other data be
;; some sort of prototype or class hence the name
(struct component (id proto))

(define (create-component id [proto #f])  
  (component id proto))

(define-syntax (define-component stx)
  (syntax-parse stx
    [(_ name [~optional given-proto])
     (with-syntax ([name? (format-id #'name "~a?" (syntax-e #'name))])
       #'(begin
           ;; this is so we can say things like Shape?, Name?, Count? on an entity
           (define (name? ent)
             (member 'name (map car (hash->list (entity-components ent)))))
           (define name (create-component 'name (~? given-proto #f)))))]))

;; list of components
;; does this need to be anything else?
(define-syntax (define-archetype stx)
  (syntax-parse stx
    [(_ name components ...)
     #'(define name
         (list components ...))]))

;; entities
;; cmpnts is a (make-immutable-hasheq)
(struct entity (id components))

;; accepts a list of components
(define/contract (add-entity! cmpnts)
  (->  (listof component?) entity?)
  ;; NOTE: this means components are unique in an entity
  ;; we might not want this in practice
  (define e (create-entity (gensym) (make-immutable-hasheq (map make-cmpnt-id-val-pair cmpnts))))
  ;; add e to world
  (when (current-world) (current-world (add-entity-to-world e (current-world))))
  e)

;; check if it's a component with data or not
(define (get-cmpnt-val cmpnt) (if (component-proto cmpnt) (component-proto cmpnt) #t))

(define (make-cmpnt-id-val-pair cmpnt) (cons (component-id cmpnt) (get-cmpnt-val cmpnt)))

(define (add-entities! cmpnts n)
  (map add-entity! (make-list n cmpnts)))

(define (create-entity id [cmpnts (λ (x) #t)])  
  (entity id cmpnts))

(define (set-entity! e expr [ref (λ (x) #f)])
  ;; if the entity has just one component then it is unambiguous
  ;; if it has more than one component then we need a reference to 
  ;; the component to set it
  (define cmpnts-hash (entity-components e))
  (define keys (hash-keys cmpnts-hash))
  (define cmpnts-length (length keys))
  (define new-cmpnts-hash
    (cond 
      [(eq? cmpnts-length 1) (hash-set cmpnts-hash (first keys) expr)]
      [ref (hash-set cmpnts-hash ref expr)]
      [else (raise "recess: attempt to set entity was ambiguous")]))
  (define new-e (entity (entity-id e) new-cmpnts-hash))
  (set-current-world-entity new-e)
  new-e)

(define (add-entity-to-world e wrld)
  (let ([current-entities (world-entities wrld)])
    (struct-copy world wrld [entities (hash-set current-entities (entity-id e) e)])))

(define (remove-entity-from-world! e wrld)
  (let ([current-entities (world-entities wrld)])
    (struct-copy world wrld [entities (hash-remove current-entities (entity-id e) e)])))

(define (add-components-to-entity e cmpnts)
  (define new-e
    (entity
     (entity-id e)
     (hash-union
      (entity-components e)
      (make-immutable-hasheq (map make-cmpnt-id-val-pair cmpnts))
      #:combine (λ (old new) new))))
  (set-current-world-entity new-e)
  new-e)

(define (remove-components-from-entity e cmpnts)
  (define to-rm (map component-id cmpnts))
  (define (keep? cmpnt-assoc) (not (member (car cmpnt-assoc) to-rm)))
  (define new-e
    (entity
     (entity-id e)
     (make-immutable-hasheq (filter keep? (hash->list (entity-components e))))))
  (set-current-world-entity new-e)
  new-e)

(define (set-current-world-entity new-e)
  (current-world
   (struct-copy world (current-world)
                [entities (hash-set (world-entities (current-world)) (entity-id new-e) new-e)])))

;; get the value of a component described by ref
;; from the entity ent
(define (get ent ref)
  (hash-ref (entity-components ent) ref))

;; worlds
;; entities are a make-immutable-hasheq
(struct world (name entities dependency-graph))

;; if we ever need to keep track of a list of worlds
;(struct universe (worlds) #:mutable)
;(define recess-universe (universe '()))

;; we can use parameters for general world managament and bookkeeping
(define current-world (make-parameter #f))
(define current-events (make-parameter (make-immutable-hasheq)))
(define start-time (make-parameter (current-seconds)))

;; initialize, iterate, terminate at some point
;; create a topological ordering of the recess
;; graph and execute the nodes in that order
(define-syntax (begin-recess stx)
  (syntax-parse stx
    [(_ (~seq #:systems system-name:id ...)
        (~seq #:initialize init-expr:expr ...)
        (~seq #:stop stop-expr:expr ...)
        (~seq #:run run-expr:id))
     #'(parameterize ([current-world
                       (world (gensym) (make-immutable-hasheq) (unweighted-graph/directed '()))]
                      [start-time (current-seconds)]
                      [current-events (poll-events (current-events))])
         (begin
           (define systems (list system-name ...))
           (define system-in-out-name-lists
             (map (λ (sys) (list (system-in sys) (system-out sys) sys)) systems))
           (define (init-func) init-expr ...)
           (define first-tsorted-world
             (recess-init
              init-func
              system-in-out-name-lists
              (world-dependency-graph (current-world))))
           (current-world (struct-copy world (current-world) [dependency-graph first-tsorted-world]))
           (run-expr (list
                      start-time
                      (λ () (and stop-expr ...))
                      current-events
                      (step-world)
                      current-world))))]))

;; do a single iteration of a world graph
;; produce the world for the next iteration
(define (step-world)
  (λ ()
    (define (step-func arg)
      (cond
        [(event? arg)
         (display "this is an event:")(display arg)(displayln (event-name arg)) arg]
        [(system? arg)
         (display "executing ")(display arg)(displayln (system-id arg))
         ;; this call returns the system for the next iteration
         (define new-system-state ((system-body arg) arg)) new-system-state]
        [else (display "unknown")
              (displayln arg)
              (raise "recess: unknown graph node type")]))
    (define new-world-graph (map step-func (world-dependency-graph (current-world))))
    (current-world (struct-copy world (current-world) [dependency-graph new-world-graph]))))

;; the idea here is to poll the events by examing the hash values
;; if the hash value is a thunk we invoke it 
(define (poll-events evnts)
  (->  hash-eq? hash-eq?)
  (define (poll event-pair)
    (if (procedure? (cdr event-pair))
        (cons (car event-pair) ((cdr event-pair)))
        event-pair))
  (make-immutable-hasheq (map poll (hash->list evnts))))

(define-syntax (because stx)
  (syntax-parse stx
    [(_ (~optional (~seq #:systems system-name:id ...))
        (~optional (~seq #:events event-expr:expr ...))
        (~optional (~seq #:entities ent-expr:expr ...)))
     #:with systems #'(~? (list system-name ...) (list))
     #:with event-exprs #'(~? (list event-expr ...) (list))
     #:with ent-exprs #'(~? (list ent-expr ...) (list))
     #'(let ([syscond (systems-condition? systems)]
             [evcond (events-condition? event-exprs)]
             [entcond (entities-condition? ent-exprs)])
         (and syscond evcond entcond))]))

;; helpers for determining world termination conditions
(define (systems-condition? systems)
  (define active-sys-ids (map system-id systems))
  (define (active-sys? sys) (member (system-id sys) active-sys-ids))
  ;; TODO compose
  (define active-systems
    (filter active-sys? (filter system? (world-dependency-graph (current-world)))))
  (define (disabled? sys) (not (system-enabled sys)))
  (andmap disabled? active-systems))

(define (events-condition? events)
  #t)

;; this is essentially trying to verify some claims about the entity
;; database; things like: is the number of player entities equal to 0?
;; for now just evaluating the expression should work
(define (entities-condition? ent-exprs)
  (let ([true? (λ (ent-expr) ent-expr)])
    (andmap true? ent-exprs)))

;; An event is an identifier [also optionally a type predicate]
;; event ideas:
;; requiring a system as an implicit event = requiring all of that system's output events
;; in addition to an implicit event matching the system's name
;; events and systems have event generics
(define-generics event-generic
  [event-generic-name event-generic])

(struct event (name zero plus)
  #:methods gen:event-generic
  [(define (event-generic-name event-generic)
     (event-name event-generic))])

(struct event:source event (input))
(struct event:sink event (output))
(struct event:transform event (f))

(define (create-event name [zero (list)] [plus (λ (x y) y)])  
  (event name zero plus))

(define (set-event key value)
  (hash-set (current-events) key value))

(define-syntax (define-event stx)
  (syntax-parse stx
    [(_ name (~optional zero) (~optional plus))
     #'(begin
         (define name (event 'name (~? zero (list)) (~? plus (λ (former new) new))))
         ;; record it and it's initial value in the events table
         (current-events (hash-set (current-events) name (event-zero name)))
         name)]))

;;; i'm imagining a library of pre-defined source events

;; image event
(define image/e (event:sink 'image/e (list) append #f))
(current-events (hash-set (current-events) image/e (list)))

;; clock event
(define clock/e (event:source 'clock/e #f #f #f))
(current-events (hash-set (current-events) clock/e #f))

;; key event
(define key/e (event:source 'key/e #f #f #f))
(current-events (hash-set (current-events) key/e #f))

;; mouse event
(define mouse/e (event:source 'mouse/e #f #f #f))
(current-events (hash-set (current-events) mouse/e #f))

;;; define-system syntax and identifier bindings

;;; let's list these in the order they should be bound/evaluated
;;; top->bottom
;; binds name as a system object and an event generic
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

;; we can use this struct to persist system values between iterations
(struct system
  (id body in state pre enabled query map reduce post out)
  #:methods gen:system-generic []
  #:methods gen:event-generic
  [(define (event-generic-name event-generic)
     (system-id event-generic))])

(define (create-system
         name
         [in #f]
         [out #f]
         [body #f]
         [state #f]
         [pre #f]
         [enabled #t]
         [query #f]
         [map  #f]
         [reduce #f]
         [post  #f])
  (system name body in state pre enabled query map reduce post out))

(define-syntax (define-system stx)
  (syntax-parse stx
    [(_ system-name:id
        (~seq #:in [evt-name:id evt:expr]) ...
        (~optional (~seq #:state [given-state-name:id initial-state:expr]))
        (~optional (~seq #:pre given-pre-name:id pre-body:expr ...))
        (~optional (~seq #:enabled? enabled?-body:expr ...))
        ;; TODO: implement this syntax class
        (~optional (~seq #:query given-entity-name:id query:expr #;query:static-query))
        (~optional (~seq #:map given-maps-name:id map-body:expr ...))
        (~optional (~seq #:reduce given-reduce-name:id zero-expr:expr reduce-body:expr ...))
        (~optional (~seq #:post post-body:expr ...))
        (~seq #:out [out-evt:expr evt-val-body:expr ...]) ...)
     #:with state-name #'(~? given-state-name default-state-name)
     #:with pre-name #'(~? given-pre-name default-pre-name)
     #:with map-name #'(~? given-maps-name default-maps-name)
     #:with reduce-name #'(~? given-reduce-name default-reduce-name)
     #:with entities-name #'(~? given-entity-name default-entity-name)
     #'(begin
         (define system-name
           (create-system
            'system-name (list evt ...) (list out-evt ...)
            ;; system body
            (λ (sys)
              (define prior-state (system-state sys))
              (define (pre-body-fun state-name evts)
                (match-define (list evt-name ...) evts)
                (~? (begin pre-body ...) (void)))
              (define (enabled-body-fun state-name pre-name evts)
                (match-define (list evt-name ...) evts)
                (~? (and enabled?-body ...) #t))
              (define (post-body-fun state-name pre-name reduce-name)
                (~? (begin post-body ...) (void)))
              (define state-0 (if prior-state prior-state (~? initial-state #f)))
              (define get-event-vals (λ (ev) (hash-ref (current-events) ev)))
              (define event-vals (map get-event-vals (filter event-generic? (list evt ...))))
              (define pre-val-0 (pre-body-fun state-0 event-vals))
              (define state-1 (if (not (void? pre-val-0)) pre-val-0 state-0))
              (define input-events (let-values ([(evt-name ...) (values evt ...)])
                                     (list evt-name ...)))
              (define enabled (enabled-body-fun state-1 pre-val-0 event-vals))
              (define entities (if enabled (~? query (list)) (list)))
              (define (map-body-fun state-name pre-name entities-name evts)
                (match-define (list evt-name ...) evts)
                (~? (map (λ (entities-name) map-body ...) entities) (void)))
              (define (reduce-body-fun state-name pre-name map-name)
                (~? (begin (foldl reduce-body zero-expr entities) ...) (void)))
              (define maps-val (if
                                enabled
                                (map-body-fun state-1 pre-val-0 entities event-vals)
                                (list)))
              (define reduce-val (reduce-body-fun state-1 pre-val-0 maps-val))
              (define post (post-body-fun state-1 pre-val-0 reduce-val))
              (define state-2 (if (not (void? post)) post state-1))
              (define (output-events-fun state-name pre-name map-name reduce-name)
                (~?
                 (begin
                   (current-events
                    (hash-set
                     (current-events)
                     out-evt
                     ;; combine events
                     (~? ((event-plus out-evt)
                          (hash-ref (current-events) out-evt)
                          (begin evt-val-body ...)) (void)))) ...)
                 (void))(void))
              (define output-events
                (if enabled
                    (output-events-fun state-2 pre-val-0 maps-val reduce-val)
                    #f))
              ;; persist the end of iteration state
              ;; this helps with checking the world termination condition
              ;; between iterations
              (define new-sys (struct-copy system system-name [state state-2] [enabled enabled]))
              (set! system-name new-sys)
              new-sys)))
         (current-events (hash-set (current-events) system-name  #f))
         system-name)]))

;; helper methods

;; stub for a way to add all the systems to a world
;; without listing them all
(define (all-defined-systems)
  'all-defined-systems)

;; get all the entities in the current world that match this archetype
;; the arguments are: first component, rest of the components
(define (lookup archetype . rest)
  (define entities (world-entities (current-world)))
  (define (archetype-match? ent)
    (subset?
     (list->set (map component-id (cons archetype rest)))
     (list->set (map car (hash->list (entity-components ent))))))
  (define matches (filter archetype-match? (map cdr (hash->list entities))))
  matches)

