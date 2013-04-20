
# one time only
# dialyzer -DDIALYZER --output_plt .depsolver.plt --build_plt --apps erts kernel stdlib crypto public_key $ELIXIR/lib/elixir/ebin $ELIXIR/lib/ex_unit/ebin

# each time
mix compile && dialyzer --quiet --no_check_plt --plt .depsolver.plt -Wunderspecs -Wunmatched_returns -Werror_handling -Wrace_conditions ebin
