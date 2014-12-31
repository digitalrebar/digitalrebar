## OpenCrowbar Data Models

The data models in OpenCrowbar are expressed in ActiveRecord

Namespacing:  Individual Barclamps can add models, but are expected to add them
contained within their own namespace.

* Example:  Foo Barclamp with a model for Attrib::FooBar with a subclass for Attrib

 > BarclampFoo::Attrib
 
 > Path = `app/models/foo/attrib_foo_bar.rb`
 
 > Class = `Foo::AttribFooBar < Attrib`


## Additional Data Model Design Information

Please refer to the following links for additional guidance:

> [OpenCrowbar Design Topics](./00100_CB2_Design_Topics.md)

> [OpenCrowbar Primary Model](./crowbar_model.md)

