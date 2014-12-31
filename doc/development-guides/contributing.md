##Contributing Code

###Fork The Code

> we assume you already have a clone of  `https://github.com/opencrowbar/core`

  1. create a personal fork of the `https://github.com/opencrowbar/core`
    1. Fork the code if you want to be able to submit changes
    1. rename your fork in Github to something like 'crowbar-core' to make it easier to track.  We'll assume that you did that in these directions
    1. remember to update your public SSH key to github
  1. setup your git identity (one time only)
    1. `git config --global user.name "user.name"`
    1. `git config --global user.email "email.address"`
  1. add a personal remote: `git remote add personal `https://github.com/[yourgitnamehere]/[crowbar-core]`
  1. you can check your remotes using `git remote -v`
  2. get the latest code from your repo `git fetch personal`

###To create a pull request

  1. make your change and commit it: `git commit -a -m "I cut and pasted this"`
  1. get the latest code from origin: `git fetch`
  1. sync your code into the trunk: `git rebase`
     1. you may have to merge changes using `git add [file]= and =git rebase --continue--`
  1. run and pass all the BDD tests, [[testing/README.md]]
  1. push your change to your personal repo in a branch: `git push personal master:[my-pull-request-branch]`
  1. from your Github fork UI, create a pull request from my-pull-request-branch

###Work on a branch

It's good practice to work on a branch instead of trunk.  That allows you to isolate several changes into distinct pulls instead of co-mingling changes.

  1. get on master using 'git checkout master'
  1. make sure you are up to date using `git fetch` and `git rebase`
  1. create a branch using `git branch [namehere]`
  1. switch to that branch using `git checkout [namehere]`
  1. work & commit
  1. when you are ready to push, use `git push personal [namehere]:[namehere]`

You can switch between branches anytime!  That allows you to help on master or work on multiple pull requests.  This flow is especially handy if your pull may take a few days to be accepted because you can work on your next item while the community does the review.  It also isolates you from changes in master.  If you need to get changes from master, use `git merge master` from your branch.


###Edit Documentation

You do NOT need a local clone to update docs!  You can edit them right from your fork on Github.  Just make the changes and then create a pull request using the Github UI.  

We _love_ Docs changes!
