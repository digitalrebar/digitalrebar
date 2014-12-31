## Creating a New OpenCrowbar Barclamp

> Note: These instructions only apply to OpenCrowbar barclamps/workloads.

Before you start, figure out the name that you want to use.  It should be short but descriptive.  You will be able to add a more descriptive name, the repo name must be unique.  We also recommend using lower case and only alpha + underbar (_) in the name.

### Starting from Template

To create a new workload, start by forking or cloning the OpenCrowbar template from [[https://github.com/opencrowbar/template]]

###Inside the template:

  1. replace "OCBTemplate" occurances with the name of your workload
    1. the replacements include the crowbar.yml, licenses, readme, and /doc files.
    1. update the /rails_engine/db/migrate/[barclamp_import] script
      1. change the name of the file to match the workload
      1. change the class and barclamp name to match the workload

  1. correct file names that are workload specific
    1. rename the _/doc/license/ocbtemplate.md_ file with the name of your workload.md
    1. rename the _/barclamps/ocbtemplate.yml_ file with the name of your workload.yml
    1. rename the _/BDD/features/ocbtemplate.feature_ file with the name of your workload.feature
    1. rename the _/roles/ocbtemplate-base/_ directory with the name of your name of your workload-base
