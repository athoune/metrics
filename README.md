Metrics
=======

Counters for erlang apps.

Two basics tools to get numbers from your application : 

Counter
_______

How many time this action happens?

```erlang
application:start(metrics),
metrics_counter:incr(my_counter, 2),
```

Gauge
-----

Collect score for mean, min/max and other statistic later
```erlang
application:start(metrics),
metrics_gauge:append(my_gauge, 42),
```

There is an helper for managing timer :

```erlang
application:start(metrics),
Start = now(),
% do something here
metrics_gauge:append_timer(my_timer, Start),
```

The future
==========

Storing value in _collectd_, _graphite_ or plain old file.