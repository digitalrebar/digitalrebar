### Adding Documentation

OpenCrowbar uses a composite documentation system that allows each barclamp to add documentation specific to its function while still building a single comprehensive documentation set.

> **Note**: Please see the Formatting subsection for tips on formatting markdown.

This information is available as a video! see [http://youtu.be/eWHeEWiOEvo](http://youtu.be/eWHeEWiOEvo)

#### Composite Documentation

It is vital to understand that the OpenCrowbar documentation system is _composite documentation._  That means that the information is assembled from multiple barclamps on the fly. This is required because the OpenCrowbar framework is really a collection of barclamps and each barclamp has its own capabilities and features.

The design of the documentation system allows each barclamp to contribute parts to the overall whole _and also_ allows parts to cross reference each other.

For example, each barclamp is expected to contribute "barclamp" and "license" information. These pages only refer to the individual barclamp's data; however, they are rolled up under the barclamp and license sections of the documentation. For OpenCrowbar suite barclamps, they are further grouped under the master OpenCrowbar set. That means that the Deployer license information depends on the OpenCrowbar Meta information.

While this adds complexity for the documentation author, it greatly simplify the documentation reading experience for the user. It also allows developers to isolate documentation changes.

#### Table of Contents - Directory Tree Layout

By design, the table of contents generally follows the directory structure of the documentation. This is intentional because it simplifies composition.

Each subdirectory can be paired with a matching topic document that functions as the index for the items in the subdirectory.

For example,

    devguide.md
    devguide/
        api.md
        api/
            node.md
            group.md
        testing.md
        testing/

In the above example, the `devguide` topic layout out general information for the developer guide. The `api` and `testing` sections would be shown as sections of the Developer Guide. Individual API topics `node` and `group` are subsections of the API topic.

If there is an index topic name (`devguide.md`) that matches a subdirectory (`devguide/`), then the contents of the subdirectory will be listed under the index topic name in the table of contents. If the index topic name is missing, but exists in the same location in the OpenCrowbar framwork docs, then the subdirectory will be conflated with other barclamps under the OpenCrowbar framework's index topic name.

##### Ordering

You can control the order of documents within a directory by prefixing the file with a number followed by an underscore.

For example, a file named `333_sample_order.md` would be ordered as 333.

> **Note**: If you omit order, the system defaults to 9999.

##### Omitting Pages

The first heading of each OpenCrowbar document must start with `#_ Title`.  If the pound (#) is omitted from the first position in the file, then it will not be included in the documentation generation process.
