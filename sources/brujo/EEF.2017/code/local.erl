-module(local).

-export [double_all/1].

double_all(List) ->
  Doubles =
    lists:map(
      fun(X) when is_number(X) -> X * 2
       ; (_) -> not_number
      end, List),
  case lists:member(not_number, Doubles) of
    true -> {error, not_all_numbers};
    false -> Doubles
  end.
