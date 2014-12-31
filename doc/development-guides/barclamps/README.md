## Crowbar Barclamps

> **NOTE: This is from the wiki and needs to be reviewed, RAH 11/9/2012**

The Crowbar barclamp provides the roles and recipes to set up the barclamp framework.
It initializes the system, creates initial instances of other barclamps defined in its configuration, and creates the users to access the crowbar API and UI. Any number of barclamp instances can be started. By default, the system starts a network, ganglia, nagios, ntp, dns, provisioner, deployer, ipmi, raid, and BIOS barclamp based upon their default configurations. The initialization function of the crowbar barclamp works exactly like the other barclamps. A proposal is created and can be committed during installation.

The main post-installation function is to provide the main transition entry point for the system. All barclamps' transition functions can be called directly, but the crowbar barclamp calls these in an order specified each barclamps' crowbar.yml file. The default unspecified priority is 1000. 

### Roles

The following node roles are defined:

   * Crowbar 
      * Configures the system to run the barclamp framework (web app and other services).
      * Depends upon the apache2, rails, passenger, and utils cookbooks.

### Scripts

The shared barclamp command line library is all the is provided to interact with the barclamp.

The following scripts are also provided.

| Script | Description |
|:----------------------|:-------------------------------------------|
| crowbar | Master control script for the command line interface |
| crowbar_crowbar | The actual control script for the crowbar barclamp |
| crowbar_watch_status |Wrapper for script that watches the node state and node status |
| crowbar_node_state | Displays the current provisioner state of the nodes |
| crowbar_node_status | Displays the current nagios state of the nodes |
| transition.sh | A helper script that can be used to transition nodes |

### Parameters

The Crowbar Barclamp has a couple of list parameters.

| Name | Default | Description |
|:---------------|:--------------------------|:--------------------------------|
| instances | The starting barclamps using their default configurations | A map of barclamp names that reference a list of json files (default is special to mean to take the defaults) that represent starting barclamp instances to create |
| users | A map of users - containing crowbar | This map defines the users allowed access to the OpenCrowbar UI and its REST API |

#### The users map contains a map. 

The key is the user name and the rest of the required fields are:

| Name | Description |
|:----------|:-----------------------------------------|
| password | Clear text password of the user |
| description | A description of the user. |

**Operations**

When the barclamp is committed, it uses a custom apply_roles function to ensure that the barclamps listed in the instances variable are created and committed.

Once running, the barclamp provides the global transition function that calls other barclamps as nodes transition. The barclamp is also responsible for creating new nodes, assigning them a temporary name. The deployer will change these things if needed later in the node life cycle. The transition function will also add the crowbar config to the admin node as it transitions through the __discovered__ state.

When starting a barclamp, use the following steps.

Example (using Swift):

**Proposals:**

   1. crowbar swift proposal list 
      - Output: Nothing or the name of the current proposals ie... default, Default, etc...
   2. crowbar swift proposal show Default &gt; swift_default.txt 
      - Output: creates the file swift_default.txt with the settings that are currently ready for deployment
      - Other things you can do with the file: 
         + Edit the file and change parameters. Once done you will need to import or edit the proposal.
   3. crowbar swift proposal --file=swift_default.txt edit Default 
      - Output: "Edited Default"
   4. Command: crowbar swift commit Default 
      - Output: "Committed Default"

**Working with the Running Config**

   1. crowbar swift list 
      - List Current running configs
   2. crowbar swift show "Name" 
      - Shows the config in question in stdout, you can use standard unix commands to send it to a file
   3. crowbar swift --file=file.txt edit "Name" 
      - Edit and commits the current running config
   4. crowbar swift create default2 
      - creates and commits a config using defaults

**Usage: crowbar swift [options] <subcommands>**

* --help or -h - help
* --hostname <name or ip> or -n <name or ip> - specifies the destination server
* --port <port> or -p <port> - specifies the destination server port
* --debug or -d - turns on debugging information
* --data <data> - used by create or edit as data (must be in json format)
* --file <file> - used by create or edit as data when read from a file (must be in json format)
* --timeout <seconds> - timeout in seconds for read http reads
* transition <name> <mac> <state> - Transition a mac to state
* edit <name> - edit a new config
* list - show a list of current configs
* help - this page
* delete <name> - delete a config
* element_node <name> - List nodes that could be that element
* elements - List elements of a deploy
* show <name> - show a specific config
* create <name> - create a specific config
* proposal - Proposal sub-commands
* commit <name> - Commit a proposal to active
* edit <name> - edit a new proposal
* list - show a list of current proposals
* delete <name> - delete a proposal
* show <name> - show a specific proposal
* create <name> - create a proposal 
