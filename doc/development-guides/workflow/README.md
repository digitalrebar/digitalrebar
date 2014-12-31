# Workflow Overview

This file documents dev, the workflow helper tool for OpenCrowbar
development.

##Assumptions:

 * Everyone has their own forks on Github of the OpenCrowbar repository
   and all of the barclamps.
 * Development workflow will involve regular synchronization
   against your upstream repositories.
 * The only path for getting code into upstream repositories is via
   pull requests.

##Requirements:

 * A checkout of OpenCrowbar.
 * Bash 4, ruby, rubygems, and the json gem.  Microsoft Windows users shoul operate in either the cygwin or the msysgw enviromnent.
 * A github username and password.

##Releases, Builds, and Barclamps:

A release is a collection of packages (RPMs or Debs), which in turn are collections of
barclamps and some associated build-specific metadata.

Releases are intended to be long-running, primary units of maintenance
and development for a collection of builds. By convention, every
release has a master build that contains references to the core
crowbar barclamps, along with other builds that may add other
barclamps to add extra capabilities.

Builds how a specific product in a release is built and what it
includes.  It includes references to barclamps, and any build-specific
metadata and infrastructure.

Barclamps enable OpenCrowbar to manage sets of services across a
cluster. All OpenCrowbar functionality is implemented in terms of
barclamps. Barclamps consist of independent git repositories with a
well-defined and dev-controlled branching structure.


##Remotes:

OpenCrowabar expects you to manage the various remotes that you work with to pull
branches for pull requests. The best way to do this is to create a new branch in
the local checkout. Checkout the new branch, work on that branch, then at the conclusion
of work push your checkout to your personal github repository. Pull requests can be
generated from the github branch to the upstream branch that the code changes are targetted to.


##Day to Day Workflows:

###Initial Setup:

* Clone the OpenCrowbar repository from you preferred upstream fork of OpenCrowbar.  
If you are not sure where to clone from, use https://github.com/opencrowbar/core.git

####Regular Development:

  1. Run _git pull_ against your upstream repositories.
    a: Git will fetch all changes from all upstream remotes for all repositories.
  2. Hack/build/test/commit.
  3. Run _git push_ to back up your changes.  This force-pushes your changes to your personal forks of the OpenCrowbar repositories on Github.
  4. If you are not ready to create a pull request for your changes, go to 1.

###Collaborating on a Feature:

####Make Feature available for fetch:
Add documentation here.

Grabbing a feature and starting to hack on it:

* Normal _git pull_ will make the feature available in your local repo.

Merging changes from parent into feature/<featurename>

  1. Commit your current work to your local repo. git commit -am 'cool message'
  2. Run _git push personal master_  returns the name of the parent of your working copy.

####Ready for pull request:
  1. make your change and commit it: 'git commit -a -m "helpful info"'
  1. get the latest code from origin: 'git fetch'
  1. sync your code into the trunk: 'git rebase'
     1. you may have to merge changes using 'git add [file]' and 'git rebase --continue--'
  1. push your change to your personal repo in a branch: 'git push personal master:[my-pull-request-branch]'
  1. from your Github fork UI, create a pull request from my-pull-request-branch

####Review pull request:
Put docs here.

####Release Workflows:
Put docs here.

####Getting a list of known releases:
Add docs here.

####Getting the release you are currently on:
Add docs here.

####Switching to a different release:
Add docs here.

All other commands operate just on your local repository.
