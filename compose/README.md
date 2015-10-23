Docker container deployment tools

# Steps to Use:
  * Make sure docker and docker-compose are installed on the system.
    * https://docs.docker.com/installation/
    * https://docs.docker.com/compose/install/
  * git clone this repo into compose
    * git clone https://github.com/rackn/container-install compose
  * Setup the parts
    * cd compose
    * ./setup
  * docker-compose up
 
# Config Changes

Some configuration can be changed through the config-dir/api/config
directory.  This gets mapped into the admin container as it boots to 
set the initial configuration.  Users and networks can altered here.

Domain name is set in the common.env file

# ToDo:
  * Convert the logging server to a service.
  * Convert Prov to cobbler.
  * Cobbler Container: Create volume/mountpoints for isos and files
    * Provisioner container maps the existing ~/.cache/digitalrebar/tftproot
      directory into place in the provisioner

