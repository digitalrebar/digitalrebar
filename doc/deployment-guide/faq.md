Q: if I add gems to the `crowbar.yml` under `gems: pkgs:` they aren't added to my install.

A: Add them in `crowbar.yml` under `gems: required_pkgs:` - those are gems pull when Crowbar is in "online" mode.  Gems in `gems: pkgs:` will be cached when "offline" mode is enabled (TBD).
