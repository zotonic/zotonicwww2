# Inspect and export Zotonic metrics

Zotonic collects metrics through Exometer. Connect to the running node with `bin/zotonic shell` and list the available metrics for a site:

```erlang
exometer:find_entries([site, garden]).
```

Use a name from that result with `exometer:info(Name, datapoints)` and `exometer:get_value(Name)`. Generate a few requests and repeat the read to confirm the metric changes. Press **Ctrl-C twice** to leave the remote shell.

Current site metrics use `[site, Site, System, What]`; the old `[zotonic, Site, webzmachine, requests]` examples are not a reliable subscription target. Counters/rates and duration histograms expose different datapoints. Discover them instead of subscribing to `value` everywhere.

For export, choose a reporter that is actually installed with your deployment. Inspect the dependency version's reporter options and configure it under `exometer_core` in `erlang.config`, then subscribe to the selected names and datapoints. Zotonic also has its own MQTT statistics reporter; see `z_stats` and `z_exometer_mqtt` for the installed implementation.

Test that the destination receives samples with the expected units, site labels and interval. Shell-only reporter changes disappear after restart. Do not assume a third-party Graphite, StatsD or SNMP reporter is bundled merely because an old cookbook listed it.
