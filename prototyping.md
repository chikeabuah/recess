This is an attempt to document the prototyping process for the recess (Racket Entity-Component-System) language. Our goal is to develop the features user interface of the recess language through iterative refinement, by working through a series of example applications.

*Process Outline*:

* Choose an example application.

* Implement it in recess, if possible. Otherwise add the necessary features to the language to implement the application.

* Analyse the implementation.

* Repair any issues found in the implementation analysis. This may involve making changes to the recess language which are not backwards compatible with previously developed example applications.

* Update the preceding examples if necessary, making sure that their previously held desirable properties still hold.

* Document as much as possible of our experiences with each iteration. Documentation content may prove useful for tutorials, blogs, etc. in the future.

*Application Ideas*

Below we outline a set of broad categories for applications that we will develop while prototyping recess:

* Playing with numbers

* Playing with pictures/images

* Playing with other forms of media (sound?)

* Simple animations with sprites

* Complex animations with sprites

* Simple simulations (self-contained, no external input)

* Complex simulations (external input from a single source)

* Complex simulations (external input from multiple sources)

* Simple games (with minimal logic/physics: like tic-tac-toe, pong)

* Complex games (such as tetris, bomberman)

*Implementation Goals (User Interface)*

The purpose of this prototyping exercise is to incrementally improve the user interface of the recess language, leaving out as many details as possible initially. Below is an attempt to outline a design philosophy for the recess user interface:

* "low floor, high ceiling" in terms of easy to get into, but hard to make not performant, and useful for building really big systems/games.

* An architecture that enables people at all experience levels to create engaging projects that they want to share, and gets people excited about programming and its possibilities.

* A language that's fun, simple, and given to rapid prototyping.

* Intuitive syntax and reliable semantics.

* Highly flexible object/entity polymorphism.

*Concrete Scenarios/Rubric*

We can start by making this a stream of thoughts that we organize and expand on as we go. We can use the prefix "ecs-" to notate entity-component-system features in the language (since the terminology can be confusing if the reader is unfamiliar; and the term “system” is ambiguous). We’ll also use the prefix for things that aren’t necessarily “vanilla ecs” but are features of the recess language, like events etc.

Scenarios

* Use an ecs-system to manipulate a single number over time (and as a function of time). The number is an ecs-entity. Try some natural variants, such as having a single ecs-system manipulate multiple ecs-entities containing atomic data values.

* Next, we can try using two ecs-systems to manipulate multiple atomic ecs-entities concurrently. Natural transition to introducing dependencies and ecs-events: why they are useful and how they can make concurrent programming intuitive?

* After playing with a few numeric/algebra examples, it should be easy to segue to scenarios involving multimedia applications: animations, simulations etc.

* Animations: a rocket sprite that takes off and comes back down to earth. How would we implement this? How can we vary the motion of the sprite: linear, parabolic, etc?

* Simulations: a small particle simulator. The particles are ecs-entities and can really be any kind of physical object: geometric shapes, dust particles etc. We want to use ecs-systems to manipulate the behavior of the particles.

Rubric

* After implementing each scenario: was this implementation as easy to write as we expected it to be? Why or why not?

* Could this implementation be more intuitive? How straightforward would it be for a beginning programmer to tell what is going on at a glance?

* Can our examples easily be extended and remixed? What do remixed programs look like, are they what we expected?

* Is the concurrency model easy for people to understand? Is it easy to implement simple and complex interactive effects with the ecs-events?

