#lang racket
(require (for-syntax syntax/parse))
(require graph)

(provide
 (all-defined-out)
 (all-from-out racket))

(define recess-graph (weighted-graph/directed '()))
(define-vertex-property recess-graph archetype)
(define-vertex-property recess-graph map-fn)
(define-vertex-property recess-graph parents #:init '())
(define-vertex-property recess-graph children #:init '())
(define-vertex-property recess-graph inputs #:init '())
(define-vertex-property recess-graph outputs #:init '())

;; A component is an identifier [and an expression]
;; Seems that components may be lambdas 
(define-syntax (define-component stx)
  (syntax-parse stx
    [(_ name [~optional body])
     (begin
       #''(name (~? body '()))
       #'(define name (gensym))
       )
     ]))

(define-syntax (define-archetype stx)
  (syntax-parse stx
    [(_ (name [arg-id default-expr] ...) components ...)
     #'(define name
         (lambda ([arg-id default-expr] ...)
           components ...))]))

;; An event is an identifier [a predicate and an expression]
(define-syntax (define-event stx)
  (syntax-parse stx
    [(_ name [~optional pred] [~optional body])
     (begin
       #''(name (~? pred '()) (~? body '()))
       #'(define name (gensym))
       )
     ]))

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
         (and (~? archetype-name #f) (archetype-set! 'system-name (~? archetype-name #f)))
         '(and (~? map-fn #f) (map-fn-set! 'system-name (~? map-fn #f)))
         (and
          (~? new-inputs #f)
          (inputs-set! 'system-name
                       (cons (~? new-inputs #f) (inputs 'system-name #:default '()))))
         (and
          (~? new-outputs #f)
          (outputs-set! 'system-name
                        (cons (~? new-outputs #f) (outputs 'system-name #:default '()))))
         (and
          (~? new-dependencies #f)
          (parents-set! 'system-name
                        (cons (~? new-dependencies #f) (parents 'system-name #:default '()))))
         (display (graphviz recess-graph)))
     ]))

