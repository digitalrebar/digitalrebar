### WSMAN Cache of Component Images

To update the build cache with new images from ftp.dell.com, you should:

* rm BUILD_CACHE_DIR/barclamps/bios/files/wsman/Catalog.xml
* From crowbar directory, run a build that contains the bios barclamp

At the end of the build, the ISO will contain updated images and a manifest to drive updates from the admin nodes as compute nodes transition through hardware-install.  The build cache will also contain these files.

