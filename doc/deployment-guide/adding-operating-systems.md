# Adding Provisionable Operating Systems

This process ensures that the Admin node can deploy operating systems to slaves.

When deploying an admin node in production mode, you will want to be able to install operating systems on slave nodes.  By default, the `provisioner-base-images` role will look for OS install ISO images in `/tftpboot/isos`.  

> If using a Docker container as an admin node and do not want to copy the ISOs into place, you must hard link (not soft link) the ISO files because symlinks are not visible on file system paths mapped inside containers

As of Dec, 2014, the provisioner knows how to install the following operating systems from the following ISO images:

 * `ubuntu-12.04`: `ubuntu-12.04.4-server-amd64.iso`
 * `ubuntu-14.04`: `ubuntu-14.04.1-server-amd64.iso`
 * `centos-6.6`: `CentOS-6.6-x86_64-bin-DVD1.iso`
 * `centos-7.0.1406`: `CentOS-7.0-1406-x86_64-DVD.iso`
 * `redhat-6.5`: `RHEL6.5-20131111.0-Server-x86_64-DVD1.iso`
 * `redhat-7.0`: `rhel-server-7.0-x86_64-dvd.iso`
 * `debian-7.8.0`: `debian-7.8.0-amd64-DVD-1.iso`

> This list is subject to change!  For the latest list, consult [Provisioner Base Images](https://github.com/opencrowbar/core/blob/master/chef/roles/provisioner-base-images/role-template.json) template file.

To enable the provisioner to install from those images, place them in `$HOME/.cache/opencrowbar/tftpboot/isos`, either directly or via a hard link.  These images will then be available inside the Docker container at `/tftpboot/isos`, and the provisioner will be able to use them to install operating systems on slave nodes.

## Add a new OS after initial annealing

If you add a new OS after the initial annealing, Crowbar must be told to rediscover available operating systems.  You must reapply (retry) the `provisioner-base-images` role (aka _Available O/S_) on the Admin server in the  System deployment.

> you can generally navigate directly to this NodeRole using `/nodes/1/node_roles/provisioner-base-images` or using the name of your admin server instead of the #1.
