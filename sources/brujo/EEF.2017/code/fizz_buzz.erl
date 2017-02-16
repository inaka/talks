-module(fizz_buzz).

-export [start/0, up_to/1].
-export [init/1, handle_call/3].

start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, noargs, []).

up_to(Number) ->
  try gen_server:call(?MODULE, {up_to, Number}) of
    ok -> ok;
    {error, not_number} ->
      io:format("Not a number ~p~n", [Number])
  catch
    _:Exception ->
      io:format("Couldn't process ~p: ~p~n", [Number, Exception])
  end.

init(noargs) -> {ok, nostate}.

handle_call({up_to, Number}, _From, State) ->
  not is_number(Number) andalso
    throw({reply, {error, not_number}, State}),

  Response = up_to(1, Number),
  {reply, Response, State}.

up_to(I, Top) when Top >= I ->
  I rem 3 == 0 andalso io:format("fizz"),
  [io:format("buzz") || I rem 5 == 0],
  I rem 3 /= 0 andalso [io:format("~p", [I]) || I rem 5 /= 0],
  io:format(" "),
  up_to(I + 1, Top);
up_to(I, Top) when Top < I ->
  io:format("~n").
