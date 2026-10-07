# Send structured logs to Logstash

The supplied `erlang.config.in` contains a `logstasher_h` Logger handler and a `logstasher` application section. Copy the relevant entries into your deployment's `erlang.config`, preserving its other Logger handlers.

Choose the receiver's host, port and supported transport (`tcp`, `udp`, or `console`) in the `logstasher` section. Enable the handler in the `kernel` Logger configuration. Use the installed sample for the handler's full formatter and level settings, rather than mixing fragments from old releases.

After applying the configuration through the deployment procedure, emit a harmless event in the remote shell:

```erlang
logger:notice(#{text => <<"Log delivery test">>, in => documentation_check}).
```

Find that event in the receiver and check its timestamp, level and structured fields. Press **Ctrl-C twice** to detach. Do not use the nonexistent `logger:error_msg/2` API.

Check behaviour when the receiver is unavailable and when buffers fill. UDP has no delivery confirmation; sending a test event is not proof the collector stored it. Keep a local diagnostic route and exclude passwords, tokens and personal message bodies from logs.
