%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Let's start with the positive part only, and check this trick?
%% Did you know parentheses were optional?
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module dumb_math.

-export [half/1].

half(Number) -> Number / 2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% What happened with that macro? OK, fine… I'll use parentheses
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(dumb_math).

-export [half/1].
-export [init/1].

half(Number) ->
  {ok, Pid} = gen_server:start_link(?MODULE, Number, []),
  sys:get_state(Pid).

init(Number) ->
  {ok, Number / 2}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% It works, but now I want to return an error if it's negative
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(dumb_math).

-export [half/1].
-export [init/1].

half(Number) ->
  {ok, Pid} = gen_server:start_link(?MODULE, Number, []),
  sys:get_state(Pid).

init(Number) ->
  if Number < 0 -> error(negative)
  end,
  {ok, Number / 2}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Ready to go... it compiles... let's try the succesful case... (BOOM)
%% OH! I have to add a true clause...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(dumb_math).

-export [half/1].
-export [init/1].

half(Number) ->
  {ok, Pid} = gen_server:start_link(?MODULE, Number, []),
  sys:get_state(Pid).

init(Number) ->
  if Number < 0 -> error(negative);
     true -> move_on
  end,
  {ok, Number / 2}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Fair enough, so now I can catch that error and return the expected string,
%% right? The docs said it's returned as {error, not_even} from gen_server
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% No luck… maybe using {stop, term()} ?
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% What's up with that exception?! Well… let's catch it!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Damn it! It's tough! Let's try to catch it, then!
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Turns out I had not to link the server! That happens to me because of
%% copy&paste
%% Now… preventing non numbers to hit me…
%% Adding that condition on the case statement is complex, I should probably use
%% an if instead... Let's first do some refactor...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(dumb_math).

-export [half/1].
-export [init/1].

half(Input) ->
  try gen_server:start(?MODULE, Input, []) of
    {error, not_even} -> io:format("~p is odd~n", [Input]);
    {ok, Pid} -> sys:get_state(Pid)
  catch
    _:Exception -> io:format("~p just happened", [Exception])
  end.

init(Input) ->
  if Input rem 2 == 0 -> {ok, Input / 2};
     true -> {stop, not_even}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Now, let's add the missing validation...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(dumb_math).

-export [half/1].
-export [init/1].

half(Input) ->
  try gen_server:start(?MODULE, Input, []) of
    {error, not_number} -> io:format("~p is not a number~n", [Input]);
    {error, not_even} -> io:format("~p is odd~n", [Input]);
    {ok, Pid} -> sys:get_state(Pid)
  catch
    _:Exception -> io:format("~p just happened", [Exception])
  end.

init(Input) ->
  if is_number(Input) and Input rem 2 == 0 -> {ok, Input / 2};
     not is_number(Input) -> {stop, not_number};
     true -> {stop, not_even}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% WAT?! WHAT HAVE JUST HAPPENED THERE?
%% operator precedence... Fixed with andalso
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(dumb_math).

-export [half/1].
-export [init/1].

half(Input) ->
  try gen_server:start(?MODULE, Input, []) of
    {error, not_number} -> io:format("~p is not a number~n", [Input]);
    {error, not_even} -> io:format("~p is odd~n", [Input]);
    {ok, Pid} -> sys:get_state(Pid)
  catch
    _:Exception -> io:format("~p just happened", [Exception])
  end.

init(Input) ->
  if is_number(Input) andalso Input rem 2 == 0 -> {ok, Input / 2};
     not is_number(Input) -> {stop, not_number};
     true -> {stop, not_even}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% but… we wanted an escript. Let's add our script called half...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#!/usr/local/bin/escript

main([Input| _]) ->
  io:format("~p", [dumb_math:half(Input)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% oh, of course! parameters are strings! we have to parse them
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#!/usr/local/bin/escript

main([Input| _]) ->
  Number = list_to_integer(Input),
  io:format("~p", [dumb_math:half(Number)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% now it works for integers... what about floats?
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#!/usr/local/bin/escript

main([Input| _]) ->
  Number = list_to_float(Input),
  io:format("~p", [dumb_math:half(Number)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% now it doesn't work for integers anymore... Not only that, it throws an
%% exception. Believe it or not, this is the right way to do this...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#!/usr/local/bin/escript

main([Input| _]) ->
  Number =
    case string:to_float(Input) of
      {error, no_float} ->
        case string:to_integer(Input) of
          {error, no_integer} -> Input;
          {Int, _Rest} -> Int
        end;
      {Float, _Rest} -> Float
    end,
  io:format("~p", [dumb_math:half(Number)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% but actually, our module only accepts integers...
%% and considering the odd vs. even limitations,
%% maybe it should produce integers as results?
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
