-module(non_local).

-export [double_all/1].

double_all(List) ->
  try lists:map(
        fun(X) when is_number(X) -> X * 2
         ; (_) -> throw(not_number)
        end, List) of
    Doubles -> {ok, Doubles}
  catch
    throw:not_number -> {error, not_all_numbers}
  end.
