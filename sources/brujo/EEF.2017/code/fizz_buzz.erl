-module fizz_buzz.

-export [up_to/1].

up_to(Number) when is_number(Number) ->
  up_to(1, Number);
up_to(NotNumber) ->
  io:format("Not a number: ~p~n", [NotNumber]), error.

up_to(I, Top) when Top >= I ->
  I rem 3 == 0 andalso io:format("fizz"),
  [io:format("buzz") || I rem 5 == 0],
  I rem 3 /= 0 andalso [io:format("~p", [I]) || I rem 5 /= 0],
  io:format(" "),
  up_to(I + 1, Top);
up_to(I, Top) when Top < I ->
  io:format("~n").
