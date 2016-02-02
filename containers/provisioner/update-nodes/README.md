update-nodes is responsible for managing provisioner bootstates for
Rebar, although it can be used in a generic fashion.

## Parameters ##

* --api-port int
    Port the HTTP API should listen on (default 8092)\
* --backend string

    Storage backend to use.  Can be either 'consul' or
    'directory' (default "consul") If 'consul', then a consul agent
    must be running on the node, and the agent must be part of a
    cluster.
* --command string

    Public URL for the Command and Control server machines should
    communicate with (default "http://localhost:3000").  update-nodes
    does not use it, but it will be passed to templates to be rendered
    as .CommandURL for scripts, kickstarts, etc. to use.
* --data-root string

    Location we should store runtime information in (default
    "digitalrebar/provisioner/boot-info").  When running with the
    'consul' backend, this will be the root in the K/V store we place
    things in.  When running with the 'directory' backend, this will
    be the directory on the local filesystem we will store information
    in.
* --file-root string

    Root of filesystem we should manage (default "/tftpboot").  This
    is where ISO images, expanded filesystems from the ISO files, and
    the expanded templates should be at.  There should also be a copy
    of lpxelinux.0 and elilo boot images in the "discovery" directory
    under that directory.
* --provisioner string

    Public URL for the provisioner (default "http://localhost:8091").
    This is the base URL of an HTTP server that serves up the contents
    of --file-root.  Note that there must also be a TFTP server
    serving the same files.

## Templates ##

update-nodes is primarly a template expansion engine.  It expands
templates written in Go's text/template language using the parameters
supplied by BootEnv and Node objects to generate the content needed to
boot and provision nodes at the various stages in their lifecycle.

### Available Template Variables ###

* .ProvisionerURL

  The URL of the provisioner that managed machines should use for
  installation and management.

* .RebarURL

  The URL of the Rebar API endpoint that managed machines should talk
  to.

* .BootParams

  Returns processed boot parameters for the boot environment.

* .Env.Name

  The name of the boot environment.

* .Env.Kernel

  Raw partial path to the kernel for the boot environment.
  You should process it into the full path appropriate to the boot
  protocol with .Env.PathFor

* .Env.Initrds

  The list of raw partial paths for the initrds for the
  boot env.  You should not use these directly, instead use the
  .Env.JoinInitrds method.

* .Env.BootParams

  The template for the boot parameters for this boot
  env.  You should not use this directly, instead you should use the
  .BootParams method.

* .Env.OS.Name

  The name of the OS that this boot env boots into.

* .Env.OS.Family

  The family of OS that this boot env boots into.

* .Env.OS.Codename

  The codename of this OS, if any.

* .Env.OS.Version

  The version of the OS, if any.

* .Env.OS.IsoFile

  The name of the downloaded ISO file.

* .Env.OS.IsoSha256

  The SHA256 of the ISO.

* .Env.OS.IsoUrl

  The URL that the ISO can be downloaded from, if applicable.

* .Machine.Name

  The FQDN of the machine.

* .Machine.Address

  The IPv4 address we expect the machine to PXE boot from.

* .Machine.HexAddress

  The IPv4 address of the machine in hexadecimal form, suitable for PXE.

* .Machine.BootEnv

  The boot environment the machine will boot with.

* .Machine.Params

The rest of the parameters that should be used by the templates.

### Template API Endpoints ###

Templates have the usual CRUD endpoints, along with a special create
endpoint that facilitates uploading larger templates.

#### Create Template (JSON) ####

POST to /templates with a body containing the following JSON:

    {
        "UUID": "a unique identifier for the template",
        "Content": "the contents of the template"
    }
        
#### Create Template (plain text) ####

POST the contents of the template to /templates/a-unique-template-name

#### List Templates ####

GET from /templates

#### Get a template ####

GET from /templates/template-UUID

#### Update a template ####

PATCH to /templates/template-UUID with a body containing a JSON patch
that describes the changes to make to the template.

#### Delete a template ####

DELETE to /templates/template-UUID

## Boot Environments ##

Boot environments (abbreviated to BootEnv) describe the environments
that nodes can boot into.  They are described with the following JSON:

    {
        "Name": "Name of the boot environment",
        "OS" : {
            "Name": "Name of the operating system this boot environment describes",
            "Family": "The family of the operating system.",
            "Codename": "The codename of the operating system",
            "Version": "The version of the operating system",
            "IsoFile": "The name of the ISO file that the OS install filesystem should be expanded from",
            "IsoSha256": "The SHA256 of the ISO file",
            "IsoUrl": "The URL that the ISO file can be downloaded from, if applicable"
        },
        "Kernel": "path/to/kernel/in/expanded/ISO",
        "Initrds": [ "path/to/initrd/1/on/ISO", "path/to/initrd/2/on/iso" ],
        "BootParams": "A text/template describing the boot parameters for the kernel this bootenv will boot",
        "RequiredParams": ["list-of","parameters_from_the","node-that-are-required","for_expansion"],
        "Templates" [
            {
                "Name": "Name of the template",
                "Path": "text/template describing how to build the path the template should be expanded to",
                "UUID": "The UUID of the template"
            },
        ]
    }
        
### Boot Environment Endpoints ###

#### Create a bootenv ####

POST to /bootenvs with a body consisting of properly-formatted JSON.

#### List bootenvs ####

GET from /bootenvs

#### Get a single bootenv ####

GET from /bootenvs/name

#### Update a bootenv ####

PATCH to /bootenvs/name with a body consisting of a JSON patch describing the changes to make

#### Delete a bootenv ####

DELETE to /bootenvs/name

## Machines ##

Machines describe the systems that the provisioner manages, along with
supplying any additional parameters that may be required by the
bootenvs for template expansion.  They are described with the following JSON:

    {
        "Name": "FQDN of the machine",
        "Address": "IPv4 address the machine will netboot with",
        "BootEnv": "The boot environment the machine will boot to",
        "Params": {
            "any-additional": "parameters",
            "the_bootenv_needs": 2,
            "know": {
                "about": "to render"
            },
            "all": [ "the", "templates" ],
            "properly": true
        }
    }

### Machine Endpoints ###

#### Create a machine ####

POST to /machines with a body consisting of properly-formatted JSON.

#### List machines ####

GET from /machines

#### Get a single machine ####

GET from /machines/name

#### Update a machine ####

PATCH to /machines/name with a body consisting of a JSON patch describing the changes to make

#### Delete a machine ####

DELETE to /machines/name
