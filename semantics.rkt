#lang racket
(require redex)

(define-language ECS
  (index ::= number)
  (identity ::= (symbol index))
  (component ::= (identity number))
  (v ::=
     x
     number
     component)
  (x ::= variable-not-otherwise-mentioned)
  (e ::=
     v
     (component-ref number)
     (+ e ...))
  (E ::= hole
     (+ v ... E e ...))
  (entity ::= (identity (component ...)))
  (entity-e ::= (identity (e ...)))
  (entity-v ::= (identity (v ...)))
  (entity-E ::= (identity (v ... E e ...)))
  (ent-map-e ::=
           (set-entity entity index e)
           (ent-map-e ...))
  (ent-map-v ::=
           (set-entity entity index v)
           (ent-map-v ...))
  (ent-map-E ::=
           (set-entity entity index E)
           (ent-map-E ...)))

