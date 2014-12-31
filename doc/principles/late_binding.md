# Ops Late Binding

In terms of computer science languages, late binding describes a class of 4th generation languages that do not require programmers to know all the details of the information they will store until the data is actually stored.  Historically, computers required very exact and prescriptive data models, but later generation languages embraced a more flexible binding.

Ops is fluid and situational.  

Many DevOps tooling leverages eventual consistency to create stable deployments.  This interative approach assumes that repeated attempts of executing the same idemnepotent scripts do deliver this result; however, they are do not deliver predictable upgrades in situations where there are circular dependencies to resolve.

Its not realistic to predict the exact configuration of a system in advance – 

  * the operational requirements recursively impact how the infrastructure is configured
  * ops environments must be highly dynamic
  * resilience requires configurations to be change tolerant

Even more complex upgrade where the steps cannot be determined in advanced because the specifics of the deployment direct the upgrade.
