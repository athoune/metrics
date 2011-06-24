Metrics
=======

Counters for erlang apps.

Two basics tools to get numbers from your application : 

Counter
-------

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

Writing datas
=============

_metrics_ can use a _writer_ to store datas.
Writer implements the behaviour _metrics_writer_.

CSV
---

```erlang
application:start(metrics),
%driver, frequency (ms), arguments
metrics:add_writer(metrics_csv, 10000, ["/tmp/my_folder"]),
metrics_counter:incr("potatoe"),
```

A cli tools is provided to compile datas

	./priv/bin/metrics_graph

The future
==========

Storing value in _collectd_, _graphite_ or plain old file.