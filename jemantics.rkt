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
  [(x y z a b c)
   variable-not-otherwise-mentioned]

  ;; Entities
  [ent-r (entity-idx)
         (component-ref number)
         (system-state)
         (event-ref number)]
  [ent-e (let ([x ent-r] ...)
           #:map (e ...)
           #:red e
           #:combine (λ (y z) e))]  

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
   (in-hole ent-ctxt ent-r-ctxt)]
  [component-ref-ctxt
   (in-hole world-ctxt sys-ctxt)]
  [entity-idx-ctxt
   (in-hole world-ctxt sys-ctxt)]
  [entity-ref-ctxt ent-r-ctxt]

  ;; entity state
  [ent-st (entity number (v ...) ent-e)
          (entity (v ...))]
  [ent-ctxt (entity number (v ...) hole)]

  ;; system state
  [sys-st (system number)
          (system v (ent-st...))
          (system
           #:sys-st v
           #:ents-to-produce-count number
           #:ents-to-delete-count number
           #:ents-to-produce-vals ((v ...) ...)
           #:ents-to-delete-indices (number ...)
           #:delete-active? boolean
           #:create-entity? boolean
           #:code sys-e 
           #:done ((v ...) ...)
           #:active ent-st
           #:rest ((v ...)
                   (v ...) ...))
          (system
           #:sys-st v
           #:do-pre! boolean)
          (system
           #:sys-st v
           #:done ((v ...) ...)
           #:do-post! boolean)]
  [sys-ctxt (system v hole)]
  
  ;; world state
  [world-st (world (v ...) sys-st)
            (world (v ...) (ent-st ...) (sys-st ...))
            (world (v ...)
                   #:done (sys-st ...)
                   #:active sys-st
                   #:rest (sys-st ...))
            (world #:done-ev (v ...)
                   #:active-ev v
                   #:rest-ev (v ...)
                   (sys-st ...))]
  [world-ctxt (world (v ...) hole)
              (world (v ...) (ent-ctxt ...) (sys-ctxt ...))]

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
   [--> (in-hole entity-idx-ctxt
                 (entity v_idx (v_c ...)
                         (in-hole entity-ref-ctxt (entity-idx))))
        (in-hole entity-idx-ctxt
                 (entity v_idx (v_c ...)
                         (in-hole entity-ref-ctxt v_idx)))]
   
   ;; xxx component-ref
   [--> (in-hole component-ref-ctxt
                 (entity v_idx (v_c ...)
                         (let ([a (component-ref number_e)])
                           #:map (e_1 ...)
                           #:red e_2)))
        (in-hole component-ref-ctxt
                 (entity v_idx (v_c ...)
                         (let ([a ,(list-ref (term (v_c ...)) (term number_e))])
                           #:map (e_1 ...)
                           #:red e_2)))]
   
   ;; system state
   [--> (in-hole world-ctxt
                 (system v_st (in-hole system-state-ctxt (system-state))))
        (in-hole world-ctxt
                 (system v_st (in-hole system-state-ctxt v_st)))]
   ;; event-ref
   [--> (world (v ...)
               (in-hole event-ref-ctxt (event-ref number_e)))
        (world (v ...)
               (in-hole event-ref-ctxt
                        ,(list-ref (term (v ...)) (term number_e))))]

   ;; xxx rule to start looking at system entities after system state pre
   [--> (system
         #:sys-st v_st
         #:code (let ([a sys-r_1] ...)
                  #:pre [b e_1]
                  #:red [c e_2 (λ (y z) e_3)]
                  #:pst e_4)
         #:rest ((v_next ...) (v_after ...) ...)
         #:do-pre! #t)
        (system
         #:sys-st e_1
         #:active (entity (v_next ...))
         #:rest ((v_after ...) ...))]
   
   ;; xxx rule to switch from one active entity inside system to next
   [--> (in-hole world-ctxt
                 (system
                  #:sys-st v_st
                  #:ent-e ent-e
                  #:done ((v_done ...) ...)
                  #:active (entity number_idx (v_before ...)
                                   (let ([x v_x] ...)
                                     #:map (v_after ...)
                                     #:red v_red
                                     #:combine (λ (b c)
                                                 e_combine)))
                  #:rest ((v_next ...)
                          (v_more ...) ...)))
        (in-hole world-ctxt
                 (system
                  #:sys-st (let ([b v_red] [c v_st])
                             e_combine)
                  #:ent-e ent-e
                  #:done ((v_after ...) (v_done ...) ...)
                  #:active (entity (+ number_idx 1) (v_next ...)
                                   ent-e)
                  #:rest ((v_more ...) ...)))]

   ;; xxx rule to switch to next system
   [--> (world (v_1 ...) 
               #:done (sys-st_done ...)
               #:active sys-st_after
               #:rest (sys-st_next sys-st_more ...))

        (world (v_1 ...)
               #:done (sys-st_after sys-st_done ...)
               #:active sys-st_next
               #:rest (sys-st_more ...))]

   ;; xxx rule to switch to next event
   [--> (world #:done-ev (v_done ...)
               #:active-ev v_after
               #:rest-ev (v_next v_more ...)
               #:done (sys-st_done ...)
               #:active sys-st_last
               #:rest ())

        (world #:done-ev (v_after v_done ...)
               #:active-ev v_next
               #:rest-ev (v_more ...)
               #:done ()
               #:active empty
               #:rest (sys-st_last sys-st_done ...))]

   ;; xxx rule to switch from one active entity inside system to next
   ;; and set up to create a new entity in the process
   [--> (in-hole world-ctxt
                 (system
                  #:sys-st v_st
                  #:ents-to-produce-count v_n1
                  #:ents-to-produce-vals ((v_to_prod ...) ...)
                  #:create-entity? #t
                  #:done ((v_done ...) ...)
                  #:red v_red
                  #:combine (λ (b c)
                              e_combine)
                  #:active (entity (v_before ...))
                  #:rest ((v_next ...)
                          (v_more ...) ...)))
        (in-hole world-ctxt
                 (system
                  #:sys-st (let ([b v_red] [c v_st])
                             e_combine)
                  #:ents-to-produce-count (+ 1 v_n1)
                  #:ents-to-produce-vals ((v_before ...) (v_to_prod ...) ...)
                  #:create-entity? #f
                  #:red v_red
                  #:combine (λ (b c)
                              e_combine)
                  #:done ((v_before ...) (v_done ...) ...)
                  #:active (entity (v_next ...))
                  #:rest ((v_more ...) ...)))]

   ;; xxx rule to switch from one active entity inside system to next
   ;; and set up to delete the active entity in the process
   [--> (in-hole world-ctxt
                 (system
                  #:sys-st v_st
                  #:ents-to-delete-count v_n1
                  #:ents-to-delete-indices (v_n2 ...)
                  #:delete-active? #t
                  #:done (v_done ...)
                  #:red v_red
                  #:combine (λ (b c)
                              e_combine)
                  #:active (entity v_before)
                  #:rest (v_next
                          v_more ...)))
        (in-hole world-ctxt
                 (system
                  #:sys-st (let ([b v_red] [c v_st])
                             e_combine)
                  #:ents-to-delete-count (+ 1 v_n1)
                  #:ents-to-delete-indices (v_before v_n2 ...)
                  #:delete-active? #f
                  #:done (v_before v_done  ...)
                  #:red v_red
                  #:combine (λ (b c)
                              e_combine)
                  #:active (entity v_next)
                  #:rest (v_more ...)))]
   
   ;; xxx rule to do system state post after last entity
   [--> (system
         #:sys-st v_st
         #:done ((v_done ...) ...)
         #:active (entity (v_before ...))
         #:red v_red
         #:combine (λ (b c)
                     e_combine)
         #:rest ())
        (system
         #:sys-st (let ([b v_red] [c v_st])
                    e_combine)
         #:done ((v_before ...) (v_done ...) ...)
         #:do-post! #t)]
   

   ;; xxx rules to switch to next system and manage entitites

   ;; entity addition
   [--> (world (v_1 ...) (ent-st_1 ...)
               #:done (sys-st_done ...)
               #:active (system
                         #:sys-st v_st
                         #:ents-to-produce-count v_n1
                         #:ents-to-produce-vals ((v_to_prod ...) (v_to_prod_next ...)...)
                         #:done ((v_done ...) ...)
                         #:active empty
                         #:rest empty)
               #:rest (sys-st_next sys-st_more ...))

        (world (v_1 ...) ((entity (v_to_prod ...)) ent-st_1 ...)
               #:done (sys-st_done ...)
               #:active (system
                         #:sys-st v_st
                         #:ents-to-produce-count (- v_n1 1)
                         #:ents-to-produce-vals ((v_to_prod_next ...)...)
                         #:done ((v_done ...) ...)
                         #:active empty
                         #:rest empty)
               #:rest (sys-st_next sys-st_more ...))]

   ;; entity deletion
   [--> (world (v_1 ...) (ent-st_1 ...)
               #:done (sys-st_done ...)
               #:active (system
                         #:sys-st v_st
                         #:ents-to-delete-count v_n1
                         #:ents-to-delete-indices (v_to_del v_to_del_next ...)
                         #:done ((v_done ...) ...)
                         #:active empty
                         #:rest empty)
               #:rest (sys-st_next sys-st_more ...))

        (world (v_1 ...) ,(remove
                           (list-ref (term (ent-st_1 ...)) (term v_to_del))
                           (term (ent-st_1 ...)))
               #:done (sys-st_done ...)
               #:active (system
                         #:sys-st v_st
                         #:ents-to-delete-count (- v_n1 1)
                         #:ents-to-delete-indices (v_to_del_next ...)
                         #:done ((v_done ...) ...)
                         #:active empty
                         #:rest empty)
               #:rest (sys-st_next sys-st_more ...))]
   
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
  

  ;; referencing

  ;; entity-idx
  (tred '(world () (system 42 (entity 42 (0 1 2)
                                      (let ([key? (entity-idx)]) #:map (1) #:red 42))))
        '(world () (system 42 (entity 42 (0 1 2) (let ([key? 42]) #:map (1) #:red 42)))))
  
  ;; component-ref
  (tred ' (world (0 1 2) (system 7 (entity 42 (0 1 2)
                                           (let ([key? (component-ref 2)]) #:map (1) #:red 42))))
        ' (world (0 1 2) (system 7 (entity 42 (0 1 2) (let ([key? 2]) #:map (1) #:red 42)))))

  ;; event-ref
  (tred '(world (0 1 2) (system 7 (entity 42 (0 1 2)
                                          (let ([key? (event-ref 0)]) #:map () #:red 42))))
        '(world (0 1 2) (system 7 (entity 42 (0 1 2) (let ([key? 0]) #:map () #:red 42)))))

  ;; system state
  (tred '(world (0 1 2) (system 7 (entity 42 (0 1 2)
                                          (let ([key? (system-state)]) #:map () #:red 42))))
        '(world (0 1 2) (system 7 (entity 42 (0 1 2) (let ([key? 7]) #:map () #:red 42)))))

  ;; xxx rule to start looking at system entities after system state pre
  (tred
   '(system
     #:sys-st 1
     #:code (let ([a (event-ref 0)])
              #:pre [b (+ 1 2)]
              #:red [c 1 (λ (y z) (+ y z))]
              #:pst 2)
     #:rest ((1 2) (3 4))
     #:do-pre! #t)
   '(system
     #:sys-st (+ 1 2)
     #:active (entity (1 2))
     #:rest ((3 4))))

  ;; switching from one active entity inside system to next
  (tred '(world (0 1 2)
                (system
                 #:sys-st 1
                 #:ent-e (let ([a (component-ref 0)])
                           #:map (a)
                           #:red a
                           #:combine (λ (b c) (+ b c)))
                 #:done ((1) (0))
                 #:active (entity 2 (2)
                                  (let ([a 2])
                                    #:map (2)
                                    #:red 2
                                    #:combine (λ (b c) (+ b c))))
                 #:rest ((3) (4))))
        '(world (0 1 2)
                (system
                 #:sys-st (let ([b 2] [c 1]) (+ b c))
                 #:ent-e (let ([a (component-ref 0)])
                           #:map (a)
                           #:red a
                           #:combine (λ (b c) (+ b c)))
                 #:done ((2) (1) (0))
                 #:active (entity (+ 2 1) (3)
                                  (let ([a (component-ref 0)])
                                    #:map (a)
                                    #:red a
                                    #:combine (λ (b c) (+ b c))))
                 #:rest ((4)))))

  ;; switch to next system
  (tred '(world (0 1)
                #:done ((system 0))
                #:active (system 1)
                #:rest ((system 2) (system 3)))

        '(world (0 1)
                #:done ((system 1) (system 0))
                #:active (system 2)
                #:rest ((system 3))))



  ;; xxx rule to switch to next event
  (tred '(world #:done-ev (0)
                #:active-ev 1
                #:rest-ev (2 3)
                #:done ((system 1) (system 0))
                #:active (system 2)
                #:rest ())

        '(world #:done-ev (1 0)
                #:active-ev 2
                #:rest-ev (3)
                #:done ()
                #:active empty
                #:rest ((system 2) (system 1) (system 0))))


  ;; xxx test switching from one active entity inside system to next
  ;; and create a new entity in the process
  (tred '(world (0 1)
                (system
                 #:sys-st 1
                 #:ents-to-produce-count 1
                 #:ents-to-produce-vals ((1 2) (3 4))
                 #:create-entity? #t
                 #:done ((1 2) (3 4))
                 #:red 1
                 #:combine (λ (b c) (+ b c))
                 #:active (entity (1 2))
                 #:rest ((1 2)
                         (3 4))))
        '(world
          (0 1)
          (system
           #:sys-st (let ((b 1) (c 1)) (+ b c))
           #:ents-to-produce-count (+ 1 1)
           #:ents-to-produce-vals ((1 2) (1 2) (3 4))
           #:create-entity? #f
           #:red 1
           #:combine (λ (b c) (+ b c))
           #:done ((1 2) (1 2) (3 4))
           #:active (entity (1 2))
           #:rest ((3 4)))))

  ;; xxx test switching from one active entity inside system to next
  ;; and set up to delete the active entity in the process
  (tred '(world
          (0 1)
          (system
           #:sys-st 1
           #:ents-to-delete-count 1
           #:ents-to-delete-indices (1)
           #:delete-active? #t
           #:done (1)
           #:red 1
           #:combine (λ (b c) (+ b c))
           #:active (entity 2)
           #:rest (3
                   4 )))
        '(world
          (0 1)
          (system
           #:sys-st (let ((b 1) (c 1)) (+ b c))
           #:ents-to-delete-count (+ 1 1)
           #:ents-to-delete-indices (2 1)
           #:delete-active? #f
           #:done (2 1)
           #:red 1
           #:combine (λ (b c) (+ b c))
           #:active (entity 3)
           #:rest (4))))

  ;; xxx test system state post after last entity
  (tred '(system
         #:sys-st 1
         #:done ((1 2))
         #:active (entity (3 4))
         #:red 1
         #:combine (λ (b c) (+ b c))
         #:rest ())
        '(system
         #:sys-st (let ((b 1) (c 1)) (+ b c))
         #:done ((3 4) (1 2))
         #:do-post! #t))


  ;; xxx test switching to next system and manage entitites

   ;; entity addition
   (tred '(world (0 1) ((entity (2 4)) (entity (1 2)))
               #:done ((system 1))
               #:active (system
                         #:sys-st 1
                         #:ents-to-produce-count 1
                         #:ents-to-produce-vals ((3 4) (5 6))
                         #:done ((6 7))
                         #:active empty
                         #:rest empty)
               #:rest ((system 3) (system 4)))

        '(world (0 1) ((entity (3 4)) (entity (2 4)) (entity (1 2)))
               #:done ((system 1))
               #:active (system
                         #:sys-st 1
                         #:ents-to-produce-count (- 1 1)
                         #:ents-to-produce-vals ((5 6 ))
                         #:done ((6 7))
                         #:active empty
                         #:rest empty)
               #:rest ((system 3) (system 4))))

   ;; entity deletion
   (tred '(world (0 1) ((entity (3 4)) (entity (2 4)) (entity (1 2)))
               #:done ((system 1))
               #:active (system
                         #:sys-st 1
                         #:ents-to-delete-count 1
                         #:ents-to-delete-indices (0 1)
                         #:done (( 2 3))
                         #:active empty
                         #:rest empty)
               #:rest ((system 3) (system 4)))

        '(world (0 1) ((entity (2 4)) (entity (1 2)))
               #:done ((system 1))
               #:active (system
                         #:sys-st 1
                         #:ents-to-delete-count (- 1 1)
                         #:ents-to-delete-indices (1)
                         #:done ((2 3))
                         #:active empty
                         #:rest empty)
               #:rest ((system 3) (system 4)))))

  