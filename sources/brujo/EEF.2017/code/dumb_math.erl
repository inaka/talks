-ifdef(vesion0).
-module dumb_math.

-export [half/1].
-export [init/1].

half(Number) ->
  {ok, Pid} = gen_server:start_link(?MODULE, Number, []),
  sys:get_state(Pid).

init(Number) ->
  {ok, Number / 2}.

%% What happened with that macro? OK, fine… I'll use parentheses
-endif.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-ifdef(version1).
-module(dumb_math).

-export [half/1].
-export [init/1].

half(Number) ->
  {ok, Pid} = gen_server:start_link(?MODULE, Number, []),
  sys:get_state(Pid).

init(Number) ->
  {ok, Number / 2}.

%% It works, but now I want to return an error if it's not even
-endif.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-ifdef(version2).
-module(dumb_math).

-export [half/1].
-export [init/1].

half(Number) ->
  {ok, Pid} = gen_server:start_link(?MODULE, Number, []),
  sys:get_state(Pid).

init(Number) ->
  case Number rem 2 of
    0 -> {ok, Number / 2};
    1 -> error(not_even)
  end.

%% Fair enough, so now I can catch that error and return the expected string,
%% right? The docs said it's returned as {error, not_even} from gen_server
-endif.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-ifdef(version3).
-module(dumb_math).

-export [half/1].
-export [init/1].

half(Number) ->
  case gen_server:start_link(?MODULE, Number, []) of
    {error, not_even} -> io:format("~p is odd~n", [Number]);
    {ok, Pid} -> sys:get_state(Pid)
  end.

init(Number) ->
  case Number rem 2 of
    0 -> {ok, Number / 2};
    1 -> error(not_even)
  end.

%% No luck… maybe using {stop, term()} ?
-endif.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-ifdef(version4).
-module(dumb_math).

-export [half/1].
-export [init/1].

half(Number) ->
  case gen_server:start_link(?MODULE, Number, []) of
    {error, not_even} -> io:format("~p is odd~n", [Number]);
    {ok, Pid} -> sys:get_state(Pid)
  end.

init(Number) ->
  case Number rem 2 of
    0 -> {ok, Number / 2};
    1 -> {stop, not_even}
  end.

%% What's up with that exception?! Well… let's catch it!
-endif.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-ifdef(version5).
-module(dumb_math).

-export [half/1].
-export [init/1].

half(Number) ->
  case catch gen_server:start_link(?MODULE, Number, []) of
    {error, not_even} -> io:format("~p is odd~n", [Number]);
    {ok, Pid} -> sys:get_state(Pid)
  end.

init(Number) ->
  case Number rem 2 of
    0 -> {ok, Number / 2};
    1 -> {stop, not_even}
  end.

%% Damn it! It's tough! Let's try to catch it, then!
-endif.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-ifdef(version5).
-module(dumb_math).

-export [half/1].
-export [init/1].

half(Number) ->
  try gen_server:start_link(?MODULE, Number, []) of
    {error, not_even} -> io:format("~p is odd~n", [Number]);
    {ok, Pid} -> sys:get_state(Pid)
  catch
    _:Exception -> io:format("~p just happened", [Exception])
  end.

init(Number) ->
  case Number rem 2 of
    0 -> {ok, Number / 2};
    1 -> {stop, not_even}
  end.

%% Turns out I had not to link the server! That happens to me because of
%% copy&paste
-endif.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%-ifdef(version6).
-module(dumb_math).

-export [half/1].
-export [init/1].

half(Number) ->
  try gen_server:start(?MODULE, Number, []) of
    {error, not_even} -> io:format("~p is odd~n", [Number]);
    {ok, Pid} -> sys:get_state(Pid)
  catch
    _:Exception -> io:format("~p just happened", [Exception])
  end.

init(Number) ->
  case Number rem 2 of
    0 -> {ok, Number / 2};
    1 -> {stop, not_even}
  end.

%% Turns out I had not to link the server!
%-endif.
