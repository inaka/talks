-module(dumb_math).

-export [half/1].
-export [init/1].

half(Input) ->
  case gen_server:start(?MODULE, Input, []) of
    {ok, Pid} -> sys:get_state(Pid);
    {error, not_number} -> io:format("~p is not a number~n", [Input]), error;
    {error, negative} -> io:format("~p is negative~n", [Input]), error
  end.

init(Input) ->
  is_number(Input) andalso Input < 0 andalso throw({stop, negative}),
  [throw({stop, not_number}) || not is_number(Input)],
  {ok, Input / 2}.
