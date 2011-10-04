-module(metrics_test).
-author('mathieu@garambrogne.net').

-compile(export_all).

setup() ->
    application:load(sasl),
    application:set_env(sasl, sasl_error_logger, {file, "metrics.log"}),
    error_logger:tty(false),
    error_logger:logfile({open, "metrics.log"}),
    ok = application:start(metrics).

cleanup(_) ->
    ok = application:stop(metrics).
