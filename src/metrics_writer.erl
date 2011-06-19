-module(metrics_writer).
-author('mathieu@garambrogne.net').

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{write_gauge,2},
     {write_counter,2},
     {write, 4}];
behaviour_info(_Other) ->
    undefined.