# Crowbar delivers "Ready State" infrastructure

The following points focus on the unique aspects of the Crowbar Operations Model.  Unlike other Operating System Provisioning Tools (like Razor, Foreman, Cobbler), Crowbar takes a systems perspective to physical infrastracture management.  Specifically, Crowbar acknowledges that successful deployment means connecting all the various operational components together including NTP, DNS and (most critically) networking from both the server and switch sides. We also believe there are no "application islands" in production data centers: operators manage Ceph and OpenStack as independent but connected applications.  While a coordinated system perspective is essential, we seek to complement existing operational tools like Chef and Puppet.

This coordinated system perspective requires that actions take place in controlled sequences both within and between data center components; consequently, Crowbar's heart is an orchestration platform designed for physical hardware.  Infrastructure has distinct challenges and needs that are unique from other cloud focused orchestration and configuration managment.  If we can solve these problems then the general cloud focused tools become highly reusable for both physical, virtual and containerized deployments.

For this reason, we see Crowbar as complementary to application provisioning tools like Fuel, Puppet, Chef, Juju, Salt or Ansible which focus on application configuration post ready state.  In these tools, the community is working to capture the specific operational characteristics of the applications.  Crowbar can be used to to simply provide ready state or taken further to drive these tools at the system level.  For example, the OpenStack Chef Cookbooks lack the system orchestration that Crowbar provides but Juju includes.

These are _not_ OpenStack focused items.  They are universal operations concerns for any scale out application including OpenStack, Ceph, Hadoop, Cloud Foundry, Cloud Stack or others.

Key Points:
* configure RAID & BIOS systems (ideally, "late bound" so target configuration is determined with application bring-up)
  * in band configuration using discovery images (as needed)
  * out of band RAID & BIOS - does not require systems to boot into discovery image for iDRAC or iLO
* UEFI boot capable to handle > 2 TB drives
* IPv6 native 
  * able to handle IPv6 communication
  * setup IPv6 DNS records when nodes are managed
  * easier to change/reconfigure IPv4
* Repeatable server-side networking config
  * able to use topology to setup bridges and bonds
  * able to correctly enumerate NICs over different hardware types and configurations
* Operating System agnostic
* CMDB agnostic (Chef & Chef Solo, Bash Scripts via SSH, Puppet, others possible)
* DevOps configuration improves flexibly and reduces maintenance over "golden" image based
  * Crowbar can deploy your golden images as you wish, and even they can notify crowbar to change hardware configs, but they must be part of the overall graph of attributes so that other workloads have a say in hardware config.

Crowbar’s key function ends up being orchestration, so it can plug in the app provisioning tools to do their specific work.  We tried to make Crowbar work as either "disolvable provisioning" or long term support in v2.  Since Crowbar uses SSH means there’s really no penalty for leaving Crowbar around and ready if you have to maintain the system; however, things won’t crash if you stop using it.  That was not true in v1 because we relied on Chef Server to do everything.
 
Crowbar v2 is focused more on ready state because we’ve heard that is the more universal problem.  In v2, we really wanted to be able to use community cookbooks – it was not productive to have crowbar only versions.  We do much better sharing operations knowledge in the OpenStack/Ceph/Hadoop/etc communities and collaborating there.   I think Crowbar brings a lot of operational value into those cookbooks but we don’t want to force adoption in order to collaborate.
