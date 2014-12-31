# Build Sledgehammer (Advanced Dev Only)

WARNING: Option step!  Usually requires multiple retries.

By default, setup will now download golden sledgehammers and this step is not needed.

> Only do this step if you want to make changes to Sledgehammer!  We recommend using the golden sledgehammer.

  1. prep for sledgehammer requirements: 
    1. ubuntu: `sudo apt-get install curl rpm rpm2cpio`
  1. from core, `tools/build_sledgehammer.sh`
    1. warning: this may take multiple attempts to complete to downloads.  Keep trying.
    2. warning: might need a better literal mirror in sledgehammer/sledgehammer.ks - see [Details]((../../workflow/dev-build-sledgehammer.md))
