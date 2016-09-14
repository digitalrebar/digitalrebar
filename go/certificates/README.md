# certificates

This repo contains two programs that use the go-common and cfssl code bases.

The first is: trust-me

This program runs similarly to cfssl's multirootca, but this one adds an endpoint to create a new root.
It also stores its data in consul for disaster recovery.

The second is: sign-it

This program is a wrap around the JSON calls to trust-me.

