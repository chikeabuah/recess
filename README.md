# Recess (Racket Entity-Component-System) 

### Goals    

The high-level goal of this project is to explore the development and performance benefits of the ECS paradigm. ECS architectures have seen a recent resurgence in games, simulations, graphics and VR recently (Unity, A-Frame, etc) because developing within this framework has the potential to improve building large systems in the following ways:

- An improved user interface for building more flexible objects (entities).
- Better performance achieved by taking advantage of opportunities for parallelism and memory compactness.

Our goal is to explore these benefits by developing a specialized syntax and semantics for ECS languages, and benchmarking their performance. In order to achieve this we have outlined the following subgoals:

 - Develop an ECS syntax and framework in Racket (Recess).
 - Develop an optimized version(s) of Recess to illustrate how we can improve upon the performance.
 - Develop a operational semantics model for our optimized ECS language (expressed in [PLT Redex](http://docs.racket-lang.org/redex/index.html)). The goal here is to formalize and test the guarantees (see below) that we are asserting about our ECS language.
 - A JavaScript port of Recess, to make it available on other platforms and explore an implementation in another host language.
  
In doing this we hope to provide definitive answers to the questions: why is ECS design useful, and what code properties will lead to fully realizing its benefits?      

### Design Schema  
This is an attempt to document the desired properties of a general (pure) ECS domain specific language. 
This ECS design makes use of the following properties:     
- Entity: Collection of components.
  - Examples: The player, the obstacles, the score display
- Component: Raw data, no logic.
  - Examples: Position, Physics, Shape
- Archetype: Unique collection of component types.
  - Examples: Collidable objects
- Event: Collection of data that can be generated and consumed by systems or by the external world. Multiple event instances must be combined into one event.
  - Examples: Input from user, Output (graphics, audio) to world, HitDetected
- System: Functions: logic, event handlers, update functions. Responsible for inspecting and updating data in components. Runs on every entity that matches its archetype.
  - Examples: Gravity, Collision Detection
- World: Collection of entities  + systems. Other forms of bookkeeping.
  - Examples: The game itself
- Universe: Support for networked, distributed worlds.
  - Examples: Many multiplayer games
  
### General
- Once a program starts running, the set of components, events, systems, archetypes, and worlds is fixed (static), but the entities and system states change over time (dynamic).
- The ECS domain specific language describes a staged computation where the components exist only statically, the entities are opaque objects, and the atomic entity data is the only runtime data.
- The DSL should always follow static scoping rules and is a shallow embedding in Racket.
  
Below are desired properties regarding individual concepts:    

### Entity
- Entities are opaque data types.
- Entities are simply containers for components. They provide the extensible nature of the ECS pattern. There are no restrictions on what components can be added to an entity.
- Entities have no dependencies.
- It is always possible to add/remove a component from an entity creation point and not have to change any other part of the program to have it compile.
- We can dynamically add/remove components from an entity at run-time with no possibility of this directly causing any run-time errors.
- In general modifying the archetype of an entity is very flexible, and should always be a safe operation.
- An entity is conceptually very similar to a database table entry. A single entity can be easily accessed via a UUID.
- Multiple entities of the same/similar archetype can easily be queried for leveraging a world (entity manager).
- We can always add a component to an entity and the same code will run, but maybe more, i.e. the code execution function is monotonic in the number of components.
- When an entity receives a new component type, it will also receive default/initial component values associated with that component.
- Entities can be destroyed at any time. Once this occurs all associated component instances (data) should also be destroyed and garbage collected.  No systems should be able to act on an entity once it is destroyed. However, if a system is currently acting on an entity when a modify/destroy command comes through, the command must block till the system is done (semaphore).

### Component
- Components are raw atomic data. A component can either be a: boolean, number, string, vector, enumeration, or a struct consisting of these types..
- Components are self contained and have no dependencies (they rely on systems to enforce any dependence). Components cannot contain references to entities or other components. 
- Components have names, which are similar to class names/types, and can be used to easily reference that type of component when constructing an archetype.
- Components must have default/initial values.
- It is always possible to add fields/attributes to any component and have the same program continue to compile and run, without requiring other changes.
- Components are always static objects.

### Events
- An event is a collection of data that can be generated and consumed by systems or by the external world.
- Event data is atomic in the same way as component data.
- An event has fields (like components) and a `combine` function that takes two events of the same type and turns them into a single instance.
- Combine example: An event could be something like (Graphics x y spr) for “Render this sprite at this location”, but you want to be able to do many of those so it should really be (Graphics (listof (x * y * spr))) and the `combine` operation just appends them all together. One implementation annoyance is that I really want to be able to avoid dynamic memory allocation and appending is very expensive in memory, so maybe there are special “output” events that advance an offset in a global array, but maybe that’s just an implementation detail.
- Implementation note: There is only a single copy of the memory for an event type in the program.

### System
- Systems are responsible for updating entities. Systems can depend on each other by consuming and generating null events that are on generated by the parent system.
- A system consists of a point-wise update function that is mapped over a collection (list, stream, etc.) of entities (but really the underlying components).
- A system has an archetype which determines the entities/components it wants to receive as input. Specifically all entities whose archetype is a superset of a system’s archetype are candidates for that system.
- Because systems operate over collections of uniform inputs (groups of components), and these inputs have no dependencies, we are essentially processing streams of completely independent data. the order in the collection should never be relevant. It should be the same as, for example, processing thousands of text files to get their word counts. We may get references to all the files in a list, but those references are completely independent, so the order doesn't matter. This stream can be broken up into chunks which are processed in parallel (inner parallelism).
- The operation of systems is a map-reduce model because we need a system to walk over all the objects and determine the scene graph to send to the rendering system or an equivalent. 
- Systems have an internal global state that can be updated once per iteration.
- System : 
  - (initial : SystemState) --- Initial state of the system
  - B --- type specific to a single iteration
  - InEvents --- A static specification of which events are inputs
  - (pre : SystemState x InEvents -> SystemState x B) --- Runs before the iteration to potentially update the state and gather some data for this iteration
  - (enabled : SystemState x B -> boolean) --- Decides if the query should be run (post will run any ways)
  - (query : Archetype) --- a specification of the entities that are needed
  - A --- the type of the map-reduce
  - (map : SystemState x B x Entity -> A) --- processes a single entity
  - (reduce : SystemState x B x seq A -> A) --- combines the results of many entities
  - OutEvents --- A static specification of which events may be output
  - (post : SystemState x B x A -> SystemState x OutEvents) --- generates the final state of the system for the iteration and then produces output
- Concern: The System definition tries very hard to be purely functional. This is good for parallelism. The main place it does not do this is within the mapping function that does stuff for each entity. Traditionally, you would just mutate the components and be allowed to add components. Is it worth trying to make this functional such that the map function can return a new (updated) entity with added/remove components? One way to think about that is for there to be a special Event that gets processed by the engine after every system runs like (EntityUpdate entity-id components-to-add components-to-remove component-fields-to-change) or even one type of event per component of the form (ComponentAdd entity-id) (ComponentRemove entity-id) (ComponentUpdate entity new-fields …) then make it so the `map` operation can output these things. Again, this might be overkill. I am particularly worried about a component getting added part way through a system’s execution that causes a concurrent system to include or exclude that entity in its execution. If the event system already deals with finding a satisfying schedule, then it can solve these problems.

### Archetype
- A major benefit of object-oriented programming is the ability to easily create very many objects of the same type, all with equivalent properties and behavior. This is achieved in the ECS paradigm using an abstraction called an Archetype (sometimes referred to as an Assemblage/Aspect/Blueprint). An archetype is simply a representation of a grouping of component types.
- Archetypes provide the flexible nature of ECS, unlike class-based OOP we can mix and match components without dealing with the complexities/ambiguities of multiple inheritance such as the “diamond problem”.
- We can check if a component or archetype is a subset of another archetype in one operation.
- Creating more than one archetype with the same set of constituent components should produce an ambiguity warning (if not an error).
- Archetypes are statically resolved, they are replaced with an actual type at compile time, rather than at run time.
- A single component can be used as an archetype.

### World
- This is the bookkeeper, system dependency manager, and execution engine.
- If a system p depends on systems Q, the executing world will ensure all Q systems must return before p runs.
- If a system’s dependencies are never fulfilled, the system will never execute.
- Cyclic dependencies will produce a compile-time error.
- We can remove/disable a system from a world at any time. However, the system must have completed it’s last atomic operation (read/write) to avoid data corruption.
- If a system is operating on a component/entity, no other system may be operating on that component at the same time. The easiest way to implement this is to have systems run synchronously.
- If two or more systems do not operate on any of the same entities, and are not related by any dependencies, they can safely be executed in parallel (outer parallelism). However, the time it takes to determine if systems can be executed in parallel may not be worth the parallelism in some cases.
- Upon destroying a world (or ending the program), all of the dynamic references/state/connections/processes will also be destroyed and GCed.

### Universe
- Support for networked, distributed worlds. Not a part of vanilla ECS, but could be useful for supporting certain applications, such as multiplayer games, IOT, networked simulations etc.
- Really a way of setting up Events that communicate across the network in a “simple” way.
- Lowest priority.
  
### Implementation
- There are few predominant ways that ECS systems are implemented.
- One is to have a giant array for each component and represent entities as indices that are common across all arrays. So entity #10 gets spot #10 in the Position array and the Gravity array, although maybe it does not have Gravity enabled.
- There is a bitmap somewhere to note which components are enabled for each entity. This is essentially another giant array.
- If there are sparse components then this can be poorly performant. (For example, imagine there are millions of blocks but only one “Active” block.)
- So another method is to have these same giant arrays per component, but entity #10 gets not a bitmap for which components are enabled, but an array giving the index into the Position array (maybe #8) and the Gravity array (maybe #11). This matches the “scatter-gather” work style.
- Perhaps it would be possible for us to allow a Component to be specified as “sparse” to enable this other behavior.
  
### Syntax/Notation Ideas    
This is a take on the user interface for a Racket ECS domain specific language.    
  
The general goals for the user interface of the DSL are:    
- Conventional and highly readable
- Low entry barrier
- Exploits the natural benefits of ECS (modular, composable, reusable)
- Intuitive. Easy to reason about.
- Makes everyone happy.
  
In the illustrations below, we attempt to build out the game of [Space Invaders](https://en.wikipedia.org/wiki/Space_Invaders).  
  
Note: the syntax below is still in flux.    
  
**Creating an entity**
```racket    
   (let ([enemies (flatten enemy-matrix)])
    (for-each
     (λ (pos)
       (add-entity!
        (list Enemy Alive CurrentOffset
              MaxOffset Polarity FirstWall
              FireDelay
              (create-component 'Position pos)
              (create-component 'Axis pos)))) 
     enemies)) 
 ```  
  
**Modifying an entity**    
```racket
;; adding a component to an existing entity
;; and
;; removing a component from an existing entity
(- (+ en Dead) Alive)
```
  
**Creating/Modifying a reusable archetype**
```racket    
;; XXX Archetypes are constructors and type signatures for systems.
;; this may seem a bit weird --- because they serve two very different purposes.    
;; It might be theoretically beautiful to have them separate, but practically it 
;; seems like you almost always want to be able to construct from the archetype    
;;  
;; XXX Perhaps archetypes are a special case of a component with “parents”. That 
;; is, ActiveTetromino is like a component that has 7 parents and none of its own 
;; fields. In practice, it would be optimized away to no fields and no runtime representation?
 (define-archetype
 (ActiveTetromino [shape (random-shape)] [color (random-color)])
 (Shape shape) (Color color)
 (Position) (Rotatable)
 (CanSoftDrop) (CanHardDrop)
 (Active)) 
 ```
  
**Creating a component**    
```racket
(define-component Axis)
(define-component MaxOffset 40)
(define-component CurrentOffset 0)
;; enemy direction flag
(define-component Polarity #t)
(define-component FirstWall #f)
(define-component Friendly)
(define-component Unfriendly)
(define-component Enemy)
(define-component Neutral)
(define-component Value 0)
(define-component Score 0)
(define-component Bullet)
(define-component Thing)
(define-component Player)
(define-component Health 100)
(define-component Alive)
(define-component Dead)
(define-component MoveRate 3)
(define-component FireDelay 200)
```
  
**Creating a system**    
- Define the system’s functions, archetype, and optionally dependencies/events etc.
- The entity a system works on contains references to instances of all the components that are mentioned in the system’s archetype.
- Let’s create a gravity system to govern the downward motion of tetrominoes over time.
- We can assume a Position component with x & y coordinate fields.
- We can also assume that once a tetromino reaches a resting point, another system will be responsible for “deactivating” it (by modifying its archetype) so gravity no longer applies. This system, a check-active system, can depend on gravity, because we want to check after every downward motion if the tetromino is still active.

```racket  
(define-system move-player
  #:in [key key/e]
  #:query player (lookup Player)
  #:map pos (get player 'Position) (move-player! player (and key (key-event-code key))))

(define-system render-player
  #:in [on-move move-player]
  #:query player (lookup Player)
  #:map pos (get player 'Position)
  #:out [image/e (draw-entities pos 'ellipse-0)])

(define-system enemy-horizontal-motion
  #:query en (lookup Enemy Alive)
  ;; increment offset, move in some direction
  #:map _ (move-enemy-h en))

(define-system render-enemies
  #:in [on-h-move enemy-horizontal-motion]
  #:query en (lookup Enemy)
  #:map pos (get en 'Position)
  #:out [image/e (draw-entities pos 'ellipse-2)])

(define-system enemies-shoot
  #:query en (lookup Enemy)
  #:map _ (shoot-from-enemy en (list Bullet Unfriendly Position)))
```  
  
**World operations**    
  
 ```racket 
(begin-recess
  #:systems
  render-player render-friendly-bullets 
  move-player friendly-bullet-motion shoot
  render-enemies enemy-impact enemy-death
  enemy-horizontal-motion enemy-bullet-motion
  enemies-shoot render-unfriendly-bullets
  render-score
  #:initialize
  ;;score
  (add-entity! (list Score))
  ;; add player(s)
  (add-entity! (list Player Position))
  ;; add enemies
  (let ([enemies (flatten enemy-matrix)])
    (for-each
     (λ (pos)
       (add-entity!
        (list Enemy Alive CurrentOffset
              MaxOffset Polarity FirstWall
              FireDelay
              (create-component 'Position pos)
              (create-component 'Axis pos)))) 
     enemies))
  #:stop #f
  #:run run/lux-mode-lambda) 
```
  
