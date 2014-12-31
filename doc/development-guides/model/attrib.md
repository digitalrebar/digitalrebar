## Attrib Model

The attrib model provides an easy way to deal with manipulating data
of interest to the OpenCrowbar framework.  Attribs in OpenCrowbar have
several functions:

* They provide a mechanism that NodeRoles can use to easily pull in data
  from other sources without having to know how to parse that other
  source's blob of JSON.
* They provide an easy-to-use REST API to let users get and set their
  data with built-in validation.
* They act as a building block for UI developers to write custom view
  templates for workloads that require them.

### Defining Attribs

Attribs are defined in the crowbar.yml files, either as part of a
Role, or at the top level of the yaml file.  Attribs defined as part
of a Role can operate on the following sources of data:

* NodeRole user data, sysdata, and wall data.
* DeploymentRole user data and wall data.
* Role template data
* Node data (via its related NodeRoles)
* Deployment data (via the deployment's DeploymentRoles)

Attribs defined at the top level of a crowbar yml file operate
strictly on Nodes, either on their discrovery data, or their hint
data.

Regardless of where Attribs are declared, they have the same layout
and fields:

    attribs:
      - name: provisioner-target_os
        description: "The operating system to install on a node"
        map: 'crowbar/target_os'
        schema:
          type: str
          required: true
          enum:
            - ubuntu-12.04
            - redhat-6.5
            - centos-6.6

#### Attrib Fields

* name: The internal name of the Attrib.  Attrib names must be
globally unique.
* description: A brief description of the data that the Attrib
manipulates.
* map: The path into a blob of JSON that the Attrib uses to find its
  data.  In this example, the map field tells the Attrib that its data
  can be found at `blob["crowbar"]["target_os"]`.  Map is also used
  when mixing attribs back together to pass them off to a Jig run.
* schema: A
  [Kwalify](http://www.kuwata-lab.com/kwalify/ruby/users-guide.html)
  schema fragment that describes what valid data for this attribute
  looks like.  Note that this is a schema fragment -- it will be
  wrapped in the following template for actual validation:

        type: map
        required: true
        mapping:
          <attrib name>:
            <schema fragment>

Attribs that do not have an associated schema cannot be updated via
the UI, the API, or the internal Attrib model.
