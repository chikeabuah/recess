#lang racket/base
(require redex/reduction-semantics)

(define-language recess
  ;; Expressions
  [v number
     boolean
     empty
     (cons v v)]
  [e v
     x
     (binop e e)
     (unop e)
     (if e e e)]
  [unop not car cdr]
  [binop + - * / < <= >= > cons]
  [(x y z)
   variable-not-otherwise-mentioned]

  ;; Entities
  [ent-r (entity-idx)
         (component-ref number)
         (system-state)
         (event-ref number)]
  [ent-e (let ([x ent-r] ...)
           #:map (e ...)
           #:red e)]  

  ;; XXX how to do deletion?
  ;; XXX how to do new?

  ;; Systems
  [sys-r (system-state)
         (event-ref number)]
  [sys-e (let ([x sys-r] ...)
           #:pre [x e]
           #:ent ent-e
           #:red [x e (λ (y z) e)]
           #:pst e)]

  ;; Contexts
  [E hole
     (binop E e)
     (binop v E)
     (unop E)
     (if E e e)]
  [ent-e-ctxt
   (let ([x v] ...)
     #:map (v ... hole e ...)
     #:red e)
   (let ([x v] ...)
     #:map (v ...)
     #:red hole)]

  [ent-r-ctxt
   (let ([x v] ... [x hole] [x ent-r] ...)
     #:map (e ...)
     #:red e)]

  [expr-ctxt
   (in-hole world-ctxt (in-hole sys-ctxt (in-hole ent-ctxt ent-e-ctxt)))]
  [event-ref-ctxt
   ;; xxx can occur inside sys-e
   (in-hole sys-ctxt (in-hole ent-ctxt ent-r-ctxt))]
  [system-state-ctxt
   ;; xxx can occur inside sys-e
   (in-hole ent-ctxt ent-r-ctxt)]

  [ent-st (entity number (v ...) ent-e)]
  [ent-ctxt (entity number (v ...) hole)]

  ;; xxx capture all entities
  [sys-st (system v ent-st)]
  [sys-ctxt (system v hole)]

  ;; xxx capture all systems and all entities
  [world-st (world (v ...) sys-st)]
  [world-ctxt (world (v ...) hole)]

  )

(define red
  (reduction-relation
   recess
   ;; XXX rules for basic expressions

   ;; boolean logic
   [--> (in-hole expr-ctxt (not #t)) (in-hole expr-ctxt #f)]
   [--> (in-hole expr-ctxt (not #f)) (in-hole expr-ctxt #t)]

   ;; conditionals
   [--> (in-hole expr-ctxt (if #t e_1 e_2))
        (in-hole expr-ctxt e_1)]
   [--> (in-hole expr-ctxt (if #f e_1 e_2))
        (in-hole expr-ctxt e_2)]

   ;; pairs
   [--> (in-hole expr-ctxt (cons v_1 v_2))
        (in-hole expr-ctxt (cons v_1 v_2))]
   [--> (in-hole expr-ctxt (car (cons v_1 v_2)))
        (in-hole expr-ctxt v_1)]
   [--> (in-hole expr-ctxt (cdr (cons v_1 v_2)))
        (in-hole expr-ctxt v_2)]
   

   ;; arithmetic
   [--> (in-hole expr-ctxt (+ number_1 number_2))
        (in-hole expr-ctxt ,(+ (term number_1) (term number_2)))]
   [--> (in-hole expr-ctxt (- number_1 number_2))
        (in-hole expr-ctxt ,(- (term number_1) (term number_2)))]
   [--> (in-hole expr-ctxt (* number_1 number_2))
        (in-hole expr-ctxt ,(* (term number_1) (term number_2)))]
   [--> (in-hole expr-ctxt (/ number_1 number_2))
        (in-hole expr-ctxt ,(/ (term number_1) (term number_2)))]

   ;; predicates
   [--> (in-hole expr-ctxt (< number_1 number_2))
        (in-hole expr-ctxt ,(< (term number_1) (term number_2)))]
   [--> (in-hole expr-ctxt (<= number_1 number_2))
        (in-hole expr-ctxt ,(<= (term number_1) (term number_2)))]
   [--> (in-hole expr-ctxt (> number_1 number_2))
        (in-hole expr-ctxt ,(> (term number_1) (term number_2)))]
   [--> (in-hole expr-ctxt (>= number_1 number_2))
        (in-hole expr-ctxt ,(>= (term number_1) (term number_2)))]

   

   ;; xxx entity-idx
   ;; xxx component-ref
   [--> (in-hole world-ctxt
                 (system v_st (in-hole system-state-ctxt (system-state))))
        (in-hole world-ctxt
                 (system v_st (in-hole system-state-ctxt v_st)))]
   [--> (world (v ...)
               (in-hole event-ref-ctxt (event-ref number_e)))
        (world (v ...)
               (in-hole event-ref-ctxt
                        ,(list-ref (term (v ...)) (term number_e))))]

   ;; xxx rule to start looking at system entities after system state pre
   ;; xxx rule to switch from one active entity inside system to next
   #;[--> (system #:code ent-e #:sys-st v_st
                  #:done ((v_done ...) ...)
                  #:active (entity number_idx (v_before ...)
                                   (let ([x v_x] ...)
                                     #:map (v_after ...)
                                     #:red v_red))
                  #:rest ((v_next ...)
                          (v_more ...) ...))
          (system #:code ent-e #:sys-st (combine v_red v_st)
                  #:done ((v_after ...) (v_done ...) ...)
                  #:active (entity (add1 number_idx) (v_next ...)
                                   ent-e)
                  #:rest ((v_more ...) ...))
          ]   
   ;; xxx rule to do system state post after last entity
   #;[--> (system #:code ent-e #:sys-st v_st
                  #:done ((v_done ...) ...)
                  #:active (entity number_idx (v_before ...)
                                   (let ([x v_x] ...)
                                     #:map (v_after ...)
                                     #:red v_red))
                  #:rest ())
          (system #:code ent-e #:sys-st (combine v_red v_st)
                  #:done ((v_after ...) (v_done ...) ...)
                  #:do-post!)
          ]
   ;; xxx rule to switch to next system

   
   
   ))

;; system-term needs to have
;; - current state
;; - all the entities (one of them is "active")

;; world-term needs to have
;; - current events
;; - all the entities
;; - all the systems (one of them is "active")

(module+ test
  (define-syntax-rule (tred x y) (test--> red x y))
  ;; basic expression + entity expression tests
  ;; boolean logic
  (tred '(world (0 1 2) (system 7 (entity 42 (0 1 2) (let () #:map ((not #t)) #:red 5))))
        '(world (0 1 2) (system 7 (entity 42 (0 1 2) (let () #:map (#f) #:red 5)))))
  (tred '(world (0 1 2) (system 7 (entity 42 (0 1 2) (let () #:map (#f) #:red (not #t)))))
        '(world (0 1 2) (system 7 (entity 42 (0 1 2) (let () #:map (#f) #:red #f)))))

  ;; conditionals
  (tred '(world (0 1 2) (system 7 (entity 42 (0 1 2) (let () #:map ((if #t 2 1)) #:red 5))))
        '(world (0 1 2) (system 7 (entity 42 (0 1 2) (let () #:map (2) #:red 5)))))
  (tred '(world (0 1 2) (system 7 (entity 42 (0 1 2) (let () #:map (#f) #:red (if #f 1 2)))))
        '(world (0 1 2) (system 7 (entity 42 (0 1 2) (let () #:map (#f) #:red 2)))))

  ;; pairs
  (tred '(world (0 1 2) (system 7 (entity 42 (0 1 2) (let () #:map ((cons 1 2)) #:red 5))))
        '(world (0 1 2) (system 7 (entity 42 (0 1 2) (let () #:map ((cons 1 2)) #:red 5)))))
  (tred '(world (0 1 2) (system 7 (entity 42 (0 1 2) (let () #:map (#f) #:red (car (cons 1 2))))))
        '(world (0 1 2) (system 7 (entity 42 (0 1 2) (let () #:map (#f) #:red 1)))))
  (tred '(world (0 1 2) (system 7 (entity 42 (0 1 2) (let () #:map (#f) #:red (cdr (cons 1 2))))))
        '(world (0 1 2) (system 7 (entity 42 (0 1 2) (let () #:map (#f) #:red 2)))))
  
  ;; arithmetic
  (tred '(world (0 1 2) (system 7 (entity 42 (0 1 2) (let () #:map ((- 2 1)) #:red 5))))
        '(world (0 1 2) (system 7 (entity 42 (0 1 2) (let () #:map (1) #:red 5)))))
  (tred '(world (0 1 2) (system 7 (entity 42 (0 1 2) (let () #:map (#f) #:red (+ 1 1)))))
        '(world (0 1 2) (system 7 (entity 42 (0 1 2) (let () #:map (#f) #:red 2)))))

  ;; predicates
  (tred '(world (0 1 2) (system 7 (entity 42 (0 1 2) (let () #:map ((< 2 1)) #:red 5))))
        '(world (0 1 2) (system 7 (entity 42 (0 1 2) (let () #:map (#f) #:red 5)))))
  (tred '(world (0 1 2) (system 7 (entity 42 (0 1 2) (let () #:map (#f) #:red (> 1 1)))))
        '(world (0 1 2) (system 7 (entity 42 (0 1 2) (let () #:map (#f) #:red #f)))))
  

  (tred '(world (0 1 2) (system 7 (entity 42 (0 1 2)
                                          (let ([key? (event-ref 0)]) #:map () #:red 42))))
        '(world (0 1 2) (system 7 (entity 42 (0 1 2) (let ([key? 0]) #:map () #:red 42)))))
  (tred '(world (0 1 2) (system 7 (entity 42 (0 1 2)
                                          (let ([key? (system-state)]) #:map () #:red 42))))
        '(world (0 1 2) (system 7 (entity 42 (0 1 2) (let ([key? 7]) #:map () #:red 42))))))
