# Windows 2012 R2 Image Support #

This directory contains the utility scripts and profiles needed to
enable Digital Rebar to install Windows 2012 R2 on to managed hosts.

## Supported Windows 2012 R2 Image Flavors ##

* Windows 2012 R2 Standard 
* Windows 2012 R2 Standard Core
* Windows 2012 R2 Datacenter
* Windows 2012 R2 Datacenter Core

## Prerequisites Required to Create Image ##

* A Windows 7, 8, 10 or Server 2008R2, 2012, or 2012R2 system to act
  as a technician workstation.
* The technician workstation must have the Windows 8.1 AIK installed
  to the default location.  You can download the AIK from:
  https://www.microsoft.com/en-us/download/details.aspx?id=39982
* A Windows 2012 R2 install ISO image.  You can download evaluation
  images from:
  https://www.microsoft.com/en-us/evalcenter/evaluate-windows-server-2012-r2
* Any drivers needed to install Windows on the target systems.

## Creating a Rebar-compatible Windows install image ##

All these steps should take place on the technician workstation, and
assume that the user is logged in to the Administrator account.  They
also assume that you are working from
`C:\Users\Administrator\Desktop\2012r2`, and they will refer to it as `2012r2`.
They will also refer to the downloaded Windows ISO as `winsrc.iso`

* Copy the entire contents of the directory containing this README into `2012r2`.

* Ensure you have `winsrc.iso` in `2012r2`.

* Copy any drivers that are required to install Windows on your target hardware 
  into 2012r2/Drivers.  You can copy any number of drivers into that directory,
  as long as they are for Windows 2012r2 amd64.  The drivers must be expanded -- 
  they should have the .inf and .sys files, and there should be one driver per 
  directory under the Drivers directory.  The drivers will be installed by the
  Add-WindowsDriver powershell cmdlet -- see the documentation for that cmdlet
  if you encounter any difficulties.  If you will be testing in a VM, you will
  need to include the appropriate drivers.  For KVM or Xen-based vms, the 
  virtio drivers from https://fedoraproject.org/wiki/Windows_Virtio_Drivers
  will come in handy.
  
* Ensure that the `build-2012r2-iso.ps1` command from this directory is
  present in `2012r2`.
  
* Open a command prompt, and cd into `2012r2`.

* Run the following command:
  `powershell -executionpolicy bypass build-2012r2-iso.ps1 winsrc.iso`
  
  This command will ask you for https:// endpoint of the Rebar admin
  server and your Rebar username and password.  It will take several
  minutes to finish, and at the end you will have an ISO named
  `rebar-windows-2012r2.iso`.  This ISO can only be used with the
  Rebar install you specified, as it embeds the install-specific
  machine-install credentials along with the address of the
  provisioner service that install uses.  If either of them change,
  you will need to rebuild the .iso.  This is a technical limitation
  that may go away in the future.
  
## Installing the Rebar-compatible Windows install image ##

These steps assume you have just finished creating the Rebar
compatible windows install image.

* Run the following command: 
  `powershell -executionpolicy bypass update-bootenv.ps1 windows-2012r2-install rebar-windows-2012r2.iso`
  
  This will upload `rebar-windows-2012r2.iso` to the Rebar
  provisioner, and update the `windows-2012r2-install` boot
  environment to use the newly-uploaded .iso.

* Edit the Windows profiles in `2012r2\profiles` to match your infrastructure.
  
  The only thing that may need changing in these profiles is the
  `operating-system-license-key` value.  The default values are GLVK
  product keys, which expect there to be a KMS volume license service
  available in your environment to complete the Windows activation
  process (as described in
  <https://technet.microsoft.com/en-us/library/jj612867(v=ws.11).aspx>).
  If you are using a different form of volume licensing, you should
  enter the appropriate volume license key there instead.
  
* Upload the profiles with the Rebar CLI.

  For each profile:
  
  `rebar profiles create - < 2012r2/profile.json`

## Using the Windows install image to install Windows on to Rebar managed nodes ##

* Update the nodes you wish to install Windows on to using the Windows profile of your choice

  `rebar nodes update node.name '{"profiles: ["your-chosen-Windows-profile"]}"`
  
* Bind the rebar-installed-node role to the node, and commit the node.

