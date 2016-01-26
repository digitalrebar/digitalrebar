#!/bin/bash

# Add provisioner-service after initial converge.
rebar nodes bind "system-phantom.internal.local" to "provisioner-service"

