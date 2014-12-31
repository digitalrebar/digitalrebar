## Short Term notes for running the OpenCrowbar in Docker

This is the TL;DR version; the full version is [here](docker-admin.md).

1. Place the OS install ISOs for OSes you want to deploy on to slaves in
  `$HOME/.cache/opencrowbar/tftpboot/isos`.  We currently support:
  1. `CentOS-6.5-x86_64-bin-DVD1.iso`
  2. `RHEL6.4-20130130.0-Server-x86_64-DVD1.iso`
  3. `ubuntu-12.04.4-server-amd64.iso`
1. Prep Environment
  1. Install Docker (do once)
  2. `sudo chmod 666 /var/run/docker.sock` (to run docker without sudo)
  3. `sudo usermod -a -G docker <your-user>` (to permanently run Docker
  without sudo)
2. To build Sledgehammer:
  1. `tools/build_sledgehammer.sh` [Details](../../workflow/dev-build-sledgehammer.md)
2. To run in development mode:
  1. `tools/docker-admin centos ./development.sh`
3. To run in production mode:
  1. `tools/docker-admin centos ./production.sh admin.cluster.fqdn`
     The first time you run this, it will take awhile as caches a few
     critical files and extracts the ISOs.
  2. `tools/kvm-slave` (to launch a KVM-based compute node)

Once Crowbar is bootstrapped (or if anything goes wrong), you will get a shell running inside a 'tmux' session, the first of which is in the container.  Exiting the shell will kill Docker.

More about tmux:

http://tmuxp.readthedocs.org/en/latest/about_tmux.html
