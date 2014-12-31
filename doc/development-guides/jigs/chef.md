### Chef Jig and Developing with Cookbooks

The Chef Jig in the Chef Barclamp allows OpenCrowbar to get data from Chef (inbound) and create roles / set attributes into Chef (outbound)

#### Adding Chef Jig into a dev system

    bundle exec rake crowbar:chef:inject_conn url=http://127.0.0.1:4000 name=admin key_file=/etc/chef/client.pem

## Developing with Chef Cookbooks

### Crowbar and Berkshelf

For each of your workloads (core, openstack, hardware, etc.) Crowbar uses a centralized Berkshelf file for for all of your cookbooks. The Berksfile is in `opencrowbar/<workload>/chef/cookbooks/Berksfile`.  Crowbar ignores Berksfiles in individual cookbooks.

Berkshelf resolves cookbook dependencies by following the Berksfile instructions for local and remote dependent cookbooks.  It stores dependencies in the Berkshelf (path.)  If you're using a Chef Server, it can upload them to the Chef Server.  If you're using chef-solo or chef-client -x, it packages them on the filesystem  and delivers them to your nodes. 

We encourage you to clone from the OpenCrowbar github repos and submit pull requests.

### Developing Cookbooks

  * Run all the following as your `crowbar` user.
  * YOU MUST put your cookbook and all your cookbooks' dependencies in the centralized Berksfile for them to get picked up and used by the Chef Jig.
```
opencrowbar/<workload>/[barclamp]/chef/cookbooks/Berksfile
```
  * You can use any of the normal sources to indicate the location of dependent cookbooks.
  * Put your custom and wrapper cookbooks in `opencrowbar/<workload>/chef/cookbooks/<my_cookbook>` 
  * The Berkshelf is located at /root/.berkshelf/  Do not edit it.  If you want to prune it of old and unnecessary versions of cookbooks, feel free to use `sudo berks shelf uninstall <cookbook> -v <version>`  The Chef Jig should replace any missing versions of cookbooks in the Berkshelf next time it runs.
  * Install dependencies:
```
$ cd <opencrowbar_root>/<workload>/[barclamp]/chef/cookbooks/
$ berks install
```
   * Optional: You might want the cookbooks you indicated as dependents and your cookbooks to be available to you for reference or running chef-solo while you're developing.  The following example will download them and put them in the right place for you.
```
$ cd <opencrowbar_root>/<workload>/[barclamp]/chef/cookbooks/
$ berks install -p ./somewhere/else/to/look/at/
```
  * If you're using Chef-Solo: Package up cookbooks for delivery.  Once you like your cookbooks, you need to package them for Crowbar to distribute to the nodes (even the Crowbar admin node)
```
$ cd <opencrowbar_root>/<workload>/[barclamp]/chef/cookbooks/
$ berks package
```
  * If you kick off the annealer again for the proper role, you'll be copying that package.tar.gz file over to your slave nodes

### Testing Cookbooks

TODO: Script this, possibly under 'tools'

   * create a test node (a kvm node is just fine)
   * add it to a deployment and add the node-role that your cookbook belongs to
   * kick off the annealer to deploy your cookbooks to a test node.

FUTURE:

   * Crowbar can help integrate your normal testing patterns.  We're considering `test-kitchen` integration.

