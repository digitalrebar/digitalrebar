# Clean-up
rm -f erl_crash.dump *.beam trace_*.txt

# Compile
erlc +debug_info *.erl
