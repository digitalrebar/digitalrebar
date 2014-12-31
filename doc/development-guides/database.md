## OpenCrowbar Database (Postgresql 9.3)

Migrating to delayed_jobs for all OpenCrowabr background processing made it
immediatly obvious that sqlite is not at all well suited to handling real
concurrency when dispatching multiple jig runs on different nodes
at a time. Postgresql is designed to be capable of handling forseeable
concurrency and HA use cases, and provides sufficient scope for future
optimizations and scalability.
