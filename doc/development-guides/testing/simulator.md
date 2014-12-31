##  OpenCrowbar Environment Simulator

> This topic uses the same infrastructure as the BDD test environment

You need a working [[devtool-build]] system.

### To use the simulator:

In your dev system, run the test server:

    ./dev tests server

In a new window, start erlang

    'cd ~/crowbar/barclamps/crowbar/BDD'
    cp example.config default.config
    [review default.config and update if needed]
    cp dev.sample dev.config
    ./linux_compile.sh
    ./linux_sim.sh

Open the OpenCrowbar UI under 'http://[dev system IP]:3000'
You can then explore and even run the Annealer!

### Interactive Mode

You can also run the simulate interactively from 'erl' using 'dev:pop().' to create machines and 'dev:unpop().' to remove them.

You can change the nodes and other information created by the simulator by editing your copy of 'dev.config'.
