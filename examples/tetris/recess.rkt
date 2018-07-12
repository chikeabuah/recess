#lang racket
(require (for-syntax syntax/parse))

(provide
 (all-defined-out)
 (all-from-out racket))
 
(define-syntax (define-component stx)
  (syntax-parse stx
   [(_ name [~optional body])
    #''(name (~? body #t))]))

(define-syntax (define-archetype stx)
  (syntax-parse stx
    (displayln stx)))

(define-syntax (define-event stx)
  (syntax-parse stx
    (displayln stx)))

(define-syntax (define-system stx)
  (syntax-parse stx
    (displayln stx)))

