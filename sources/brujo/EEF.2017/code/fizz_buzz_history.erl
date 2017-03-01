-module(fizz_buzz_history).

-ifdef(nothing).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Look, ma! No parentheses!!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module fizz_buzz.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Let's start with an easy task: let's just print all numbers!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module fizz_buzz.

-export [up_to/1].

up_to(Number) ->
  up_to(1, Number).

up_to(I, Top) when Top >= I ->
  io:format("~p ", [I]),
  up_to(I + 1, Top);
up_to(I, Top) when Top < I ->
  io:format("~n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Before moving on, let's deal with some edge cases. Let's leave non-numbers
%% out fo this...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module fizz_buzz.

-export [up_to/1].

up_to(Number) ->
  up_to(1, Number).

up_to(I, Top) when is_number(Top) and Top >= I ->
  io:format("~p ", [I]),
  up_to(I + 1, Top);
up_to(I, Top) when is_number(Top) and Top < I ->
  io:format("~n");
up_to(_, Top) ->
  io:format("Not a number: ~p~n", [Top]), error.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% WAT?! 10 is not a number?
%% ...anybody can tell me what's going on here?
%% Yeah, we need our very old friend: andalso!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module fizz_buzz.

-export [up_to/1].

up_to(Number) ->
  up_to(1, Number).

up_to(I, Top) when is_number(Top) andalso Top >= I ->
  io:format("~p ", [I]),
  up_to(I + 1, Top);
up_to(I, Top) when is_number(Top) andalso Top < I ->
  io:format("~n");
up_to(_, Top) ->
  io:format("Not a number: ~p~n", [Top]), error.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Actually, let's move it to the exported function to reduce complexity...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module fizz_buzz.

-export [up_to/1].

up_to(Number) when is_number(Number) ->
  up_to(1, Number);
up_to(NotNumber) ->
  io:format("Not a number: ~p~n", [NotNumber]), error.

up_to(I, Top) when Top >= I ->
  io:format("~p ", [I]),
  up_to(I + 1, Top);
up_to(I, Top) when Top < I ->
  io:format("~n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Now let's add the fizz buzzing...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module fizz_buzz.

-export [up_to/1].

up_to(Number) when is_number(Number) ->
  up_to(1, Number);
up_to(NotNumber) ->
  io:format("Not a number: ~p~n", [NotNumber]), error.

up_to(I, Top) when Top >= I ->
  if I rem 3 == 0 ->
    io:format("fizz")
  end,
  if I rem 5 == 0 ->
    io:format("buzz")
  end,
  if I rem 3 /= 0 andalso I rem 5 /= 0 ->
    io:format("~p", [I])
  end,
  io:format(" "),
  up_to(I + 1, Top);
up_to(I, Top) when Top < I ->
  io:format("~n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% There you go, some might say I'm a little _imperative_, but it should work, right?
%% NO
%% no true branch... let's add those... ROK-style!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module fizz_buzz.

-export [up_to/1].

up_to(Number) when is_number(Number) ->
  up_to(1, Number);
up_to(NotNumber) ->
  io:format("Not a number: ~p~n", [NotNumber]), error.

up_to(I, Top) when Top >= I ->
  if I rem 3 == 0 ->
      io:format("fizz")
   ; true -> move_on
  end,
  if I rem 5 == 0 ->
      io:format("buzz")
   ; true -> nothing_to_do_here
  end,
  if I rem 3 /= 0 andalso I rem 5 /= 0 ->
      io:format("~p", [I])
   ; true -> watcha_looking_at
  end,
  io:format(" "),
  up_to(I + 1, Top);
up_to(I, Top) when Top < I ->
  io:format("~n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% It works, but those true branches are awful, luckily somebody came up with a
%% great idea on how to remove them. Actually 2 ideas (mindblown).
%% For the first one... let's remember how andalso works... etc etc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module fizz_buzz.

-export [up_to/1].

up_to(Number) when is_number(Number) ->
  up_to(1, Number);
up_to(NotNumber) ->
  io:format("Not a number: ~p~n", [NotNumber]), error.

up_to(I, Top) when Top >= I ->
  I rem 3 == 0 andalso io:format("fizz"),
  if I rem 5 == 0 ->
      io:format("buzz")
   ; true -> nothing_to_do_here
  end,
  if I rem 3 /= 0 andalso I rem 5 /= 0 ->
      io:format("~p", [I])
   ; true -> watcha_looking_at
  end,
  io:format(" "),
  up_to(I + 1, Top);
up_to(I, Top) when Top < I ->
  io:format("~n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% I hate that one... it's super ugly. There is another one using list
%% comprehensions... my 2nd-favourite Erlang structure (behind Bin Comps)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module fizz_buzz.

-export [up_to/1].

up_to(Number) when is_number(Number) ->
  up_to(1, Number);
up_to(NotNumber) ->
  io:format("Not a number: ~p~n", [NotNumber]), error.

up_to(I, Top) when Top >= I ->
  I rem 3 == 0 andalso io:format("fizz"),
  [io:format("buzz") || I rem 5 == 0],
  if I rem 3 /= 0 andalso I rem 5 /= 0 ->
      io:format("~p", [I])
   ; true -> watcha_looking_at
  end,
  io:format(" "),
  up_to(I + 1, Top);
up_to(I, Top) when Top < I ->
  io:format("~n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% You can even combine both...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module fizz_buzz.

-export [up_to/1].

up_to(Number) when is_number(Number) ->
  up_to(1, Number);
up_to(NotNumber) ->
  io:format("Not a number: ~p~n", [NotNumber]), error.

up_to(I, Top) when Top >= I ->
  I rem 3 == 0 andalso io:format("fizz"),
  [io:format("buzz") || I rem 5 == 0],
  I rem 3 /= 0 andalso
    [io:format("~p", [I]) || I rem 5 /= 0],
  io:format(" "),
  up_to(I + 1, Top);
up_to(I, Top) when Top < I ->
  io:format("~n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ...but to be a real erlanger I must use OTP, right?
%% let's use a gen_server here!
%% CHECK THE DOCS… etc… etc…
%% init is wrong… we'll fix that in the future
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module fizz_buzz.

-export [up_to/1].
-export [init/1].

up_to(Number) ->
  case gen_server:start_link(?MODULE, Number, []) of
    {ok, _Pid} -> ok;
    {error, not_number} ->
      io:format("Not a number ~p~n", [Number])
  end.

init(Number) when is_number(Number) ->
  up_to(1, Number),
  {ok, Number};
init(_NotNumber) ->
  {stop, not_number}.

up_to(I, Top) when Top >= I ->
  I rem 3 == 0 andalso io:format("fizz"),
  [io:format("buzz") || I rem 5 == 0],
  I rem 3 /= 0 andalso
    [io:format("~p", [I]) || I rem 5 /= 0],
  io:format(" "),
  up_to(I + 1, Top);
up_to(I, Top) when Top < I ->
  io:format("~n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% you see? that's why I hate macros :P
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(fizz_buzz).

-export [up_to/1].
-export [init/1].

up_to(Number) ->
  case gen_server:start_link(?MODULE, Number, []) of
    {ok, _Pid} -> ok;
    {error, not_number} ->
      io:format("Not a number ~p~n", [Number])
  end.

init(Number) when is_number(Number) ->
  up_to(1, Number),
  {ok, Number};
init(_NotNumber) ->
  {stop, not_number}.

up_to(I, Top) when Top >= I ->
  I rem 3 == 0 andalso io:format("fizz"),
  [io:format("buzz") || I rem 5 == 0],
  I rem 3 /= 0 andalso
    [io:format("~p", [I]) || I rem 5 /= 0],
  io:format(" "),
  up_to(I + 1, Top);
up_to(I, Top) when Top < I ->
  io:format("~n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Now it works for numbers… but what about the errors?
%% That was an exception, ok... let's try to catch it...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(fizz_buzz).

-export [up_to/1].
-export [init/1].

up_to(Number) ->
  try gen_server:start_link(?MODULE, Number, []) of
    {ok, _Pid} -> ok;
    {error, not_number} ->
      io:format("Not a number ~p~n", [Number])
  catch
    _:Ex ->
      io:format("Couldn't process ~p: ~p~n", [Number, Ex])
  end.

init(Number) when is_number(Number) ->
  up_to(1, Number),
  {ok, Number};
init(_NotNumber) ->
  {stop, not_number}.

up_to(I, Top) when Top >= I ->
  I rem 3 == 0 andalso io:format("fizz"),
  [io:format("buzz") || I rem 5 == 0],
  I rem 3 /= 0 andalso
    [io:format("~p", [I]) || I rem 5 /= 0],
  io:format(" "),
  up_to(I + 1, Top);
up_to(I, Top) when Top < I ->
  io:format("~n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% No luck! (try stuff on the console... still no luck)
%% Turns out, I needed to start the server not link it!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(fizz_buzz).

-export [up_to/1].
-export [init/1].

up_to(Number) ->
  try gen_server:start(?MODULE, Number, []) of
    {ok, _Pid} -> ok;
    {error, not_number} ->
      io:format("Not a number ~p~n", [Number])
  catch
    _:Ex ->
      io:format("Couldn't process ~p: ~p~n", [Number, Ex])
  end.

init(Number) when is_number(Number) ->
  up_to(1, Number),
  {ok, Number};
init(_NotNumber) ->
  {stop, not_number}.

up_to(I, Top) when Top >= I ->
  I rem 3 == 0 andalso io:format("fizz"),
  [io:format("buzz") || I rem 5 == 0],
  I rem 3 /= 0 andalso
    [io:format("~p", [I]) || I rem 5 /= 0],
  io:format(" "),
  up_to(I + 1, Top);
up_to(I, Top) when Top < I ->
  io:format("~n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% OK, OK, I hear you people... that's not how gen_servers work.
%% Let's make it right!
%% ...quick note here on nothing...
%% and I move all the logic to handle_call
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(fizz_buzz).

-export [start/0, up_to/1].
-export [init/1, handle_call/3].

start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, nothing, []).

up_to(Number) ->
  try gen_server:call(?MODULE, {up_to, Number}) of
    ok -> ok;
    {error, not_number} ->
      io:format("Not a number ~p~n", [Number])
  catch
    _:Ex ->
      io:format("Couldn't process ~p: ~p~n", [Number, Ex])
  end.

init(nothing) -> {ok, empty_state}.

handle_call({up_to, Number}, _From, State) ->
  not is_number(Number) andalso
    throw({error, not_number}),

  Response = up_to(1, Number),
  {reply, Response, State}.

up_to(I, Top) when Top >= I ->
  I rem 3 == 0 andalso io:format("fizz"),
  [io:format("buzz") || I rem 5 == 0],
  I rem 3 /= 0 andalso
    [io:format("~p", [I]) || I rem 5 /= 0],
  io:format(" "),
  up_to(I + 1, Top);
up_to(I, Top) when Top < I ->
  io:format("~n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Yeah, sure... we can't just use trhow for assertions on handle_call...
%% ...or can we?
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(fizz_buzz).

-export [start/0, up_to/1].
-export [init/1, handle_call/3].

start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, nothing, []).

up_to(Number) ->
  try gen_server:call(?MODULE, {up_to, Number}) of
    ok -> ok;
    {error, not_number} ->
      io:format("Not a number ~p~n", [Number])
  catch
    _:Ex ->
      io:format("Couldn't process ~p: ~p~n", [Number, Ex])
  end.

init(nothing) -> {ok, empty_state}.

handle_call({up_to, Number}, _From, State) ->
  not is_number(Number) andalso
    throw({reply, {error, not_number}, State}),

  Response = up_to(1, Number),
  {reply, Response, State}.

up_to(I, Top) when Top >= I ->
  I rem 3 == 0 andalso io:format("fizz"),
  [io:format("buzz") || I rem 5 == 0],
  I rem 3 /= 0 andalso
    [io:format("~p", [I]) || I rem 5 /= 0],
  io:format(" "),
  up_to(I + 1, Top);
up_to(I, Top) when Top < I ->
  io:format("~n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TADA!!!
%% Now, the final step, let's turn it into a script
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#!/usr/local/bin/escript

main([]) ->
  io:format("Usage ./fizz_buzz [number]");
main([Input| _]) ->
  {ok, _} = fizz_buzz:start(),
  fizz_buzz:up_to(Input).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Oh, right! We have to parse the input!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#!/usr/local/bin/escript

main([]) ->
  io:format("Usage ./fizz_buzz [number]");
main([Input| _]) ->
  {ok, _} = fizz_buzz:start(),
  fizz_buzz:up_to(list_to_integer(Input)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% But if I try with something that's not a number... boom!
%% list_to_integer throws an exception, let's use string:to_integer, insetead!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#!/usr/local/bin/escript

main([]) ->
  io:format("Usage ./fizz_buzz [number]");
main([Input| _]) ->
  {ok, _} = fizz_buzz:start(),
  ParsedInput =
    case string:to_integer(Input) of
      {error, no_integer} -> Input;
      {Int, _Rest} -> Int
    end,
  fizz_buzz:up_to(ParsedInput).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Yeah, but... our fizzbuzz script should work with floats, too...
%% Piece of cake, right?
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#!/usr/local/bin/escript

main([]) ->
  io:format("Usage ./fizz_buzz [number]");
main([Input| _]) ->
  {ok, _} = fizz_buzz:start(),
  ParsedInput =
    case string:to_float(Input) of
      {error, no_float} -> Input;
      {Float, _Rest} -> Float
    end,
  fizz_buzz:up_to(ParsedInput).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 100 not a number? REALLY?
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#!/usr/local/bin/escript

main([]) ->
  io:format("Usage ./fizz_buzz [number]");
main([Input| _]) ->
  {ok, _} = fizz_buzz:start(),
  ParsedInput =
    case string:to_float(Input) of
      {error, no_float} ->
        case string:to_integer(Input) of
          {error, no_integer} -> Input;
          {Int, _Rest} -> Int
        end;
      {Float, _Rest} -> Float
    end,
  fizz_buzz:up_to(ParsedInput).

-endif.
