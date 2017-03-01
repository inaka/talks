-module(fizz_buzz).

-export [start/0, up_to/1].
-export [init/1, handle_call/3, terminate/2].

start() ->
  gen_server:start(
    {local, ?MODULE}, ?MODULE, nothing, []).

up_to(N) ->
  try gen_server:call(?MODULE, {up_to, N}) of
    ok -> ok;
    {error, not_number} ->
      io:format("Not a number: ~p~n", [N])
  catch
    _:Ex ->
      io:format("Couldn't process ~p: ~p~n", [N, Ex])
  end.

init(nothing) -> {ok, empty_state}.

handle_call({up_to, Number}, _From, State) ->
  not is_number(Number) andalso
    throw({reply, {error, not_number}, State}),

  Response = up_to(1, Number),
  {reply, Response, State}.

terminate(Reason, _State) ->
  io:format(
    "Server terminating with reason: ~p~n", [Reason]).

up_to(I, Top) when Top >= I ->
  I rem 3 == 0 andalso io:format("fizz"),
  [io:format("buzz") || I rem 5 == 0],
  catch if I rem 3 /= 0 andalso I rem 5 /= 0 ->
      io:format("~p", [I])
  end,
  io:format(" "),
  up_to(I + 1, Top);
up_to(I, Top) when Top < I ->
  io:format("~n").
