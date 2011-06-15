-module(metrics_math).
-author('mathieu@garambrogne.net').

-export([
    mean/1,
    min_max/1,
    percentile/2
]).

-spec mean(list(number())) -> number().
mean(Values) ->
    Sum = lists:foldl(fun(T, Acc) ->
            T+Acc
        end, 0, Values),
    Sum / length(Values).

-spec min_max(list(number())) -> tuple(number(), number()).
min_max(Values) ->
    [Head | Tail] = Values,
    {Min, Max} = lists:foldl(fun(T, {Lmin, Lmax}) ->
        Tmin = case T < Lmin of
            true -> T;
            _ -> Lmin
        end,
        Tmax = case T > Lmax of
            true -> T;
            _ -> Lmax
        end,
        {Tmin, Tmax}
        end, {Head, Head}, Tail),
    {Min, Max}.

-spec percentile(list(number()), number()) -> number().
percentile(Values, Percentile) ->
    G = lists:sort(Values),
    case Percentile of
        100 ->
            lists:last(G);
        _ ->
            lists:nth(round(length(G)  * Percentile / 100 + 0.5), G)
    end.