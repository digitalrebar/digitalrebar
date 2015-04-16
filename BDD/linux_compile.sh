
# Clean-up
rm -f erl_crash.dump *.beam trace_*.txt

# setup configuration for local use (if missing)
if [[ ! -f default.config ]]; then
  cp example.config default.config
  cp dev.sample dev.config
fi

# Compile
erlc +debug_info *.erl