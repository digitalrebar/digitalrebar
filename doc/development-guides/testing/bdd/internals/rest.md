#### BDD Object Management

BDD follows a pattern in which test objects are use erlang modules to implement each model.   For example, the node model is expressed in node.erl.  There are standard methods that are expected for each object so that they can be correctly handled by =bdd_restrat= and =bdd_crud= modules.

> To handle objects that have name conflicts (e.g.: group, user), BDD uses an alias system where a model can be registered to use an alias.  For example, group is handled by the group_cb model.  This is configurated in the setup steps.

##### BDD CRUD

* create
* read
* update
* delete

##### BDD Rest Rat
