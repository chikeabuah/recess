#lang racket
(require (for-syntax syntax/parse))
(require graph)

(provide
 (all-defined-out)
 (all-from-out racket))

;; A component is an identifier [and an expression]
;; Seems that components may be lambdas 
(define-syntax (define-component stx)
  (syntax-parse stx
   [(_ name [~optional body])
    #''(name (~? body '()))]))


(define-syntax (define-archetype stx)
  (syntax-parse stx
   [(_ (name [arg-id default-expr] ...) components ...)
    #''(define name
         (lambda ([arg-id default-expr] ...)
           (components ...)))]))

;; An event is an identifier [a predicate and an expression]
(define-syntax (define-event stx)
  (syntax-parse stx
   [(_ name [~optional pred] [~optional body])
    #''(name (~? pred '()) (~? body '()))]))


(define define-system
  (lambda
      (system-name
       #:archetype [archetype-name null]
       #:on [inputs null]
       #:out [outputs null]
       #:depends [dependencies null]
       #:map [map-fn null])
    (string-append "Hello, " " ")))
