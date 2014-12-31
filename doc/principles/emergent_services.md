## Emergent services

We see data center operations as a duel between conflicting priorities.  On one hand, the environment is constantly changing and systems must adapt quickly to these changes.  On the other hand, users of the infrastructure expect it to provide stable and consistent services for consumption.  We’ve described that as “always ready, never finished.”

Our solution to this duality to expect that the infrastructure Crowbar builds is decomposed into well-defined service layers that can be (re)assembled dynamically.  Rather than require any component of the system to be in a ready state, Crowbar design principles assume that we can automate the construction of every level of the infrastructure from bios to network and application.  Consequently, we can hold off (re)making decisions at the bottom levels until we’ve figured out that we’re doing at the top.  

Effectively, we allow the overall infrastructure services configuration to evolve or emerge based on the desired end use.  These concepts are built on computer science principles that we have appropriated for Ops use; since we also subscribe to Opscode “infrastructure as code”, we believe that these terms are fitting in a DevOps environment.  In the next pages, we’ll explore the principles behind this approach including concepts around simulated annealing, late binding, attribute injection and emergent design.

Emergent (aka iterative or evolutionary) design challenges the traditional assumption that all factors must be known before starting 

  * Dependency graph – multidimensional relationship
  * High degree of reuse via abstraction and isolation of service boundaries.
  * Increasing complexity of deployments means more dependencies
  * Increasing revision rates of dependencies but with higher stability of APIs
