# Tune server limits from measured load

Start with a working deployment and measure open files, connections, memory, CPU, latency and database-pool pressure under representative load. Check the limits of the **service process**, not only your interactive shell.

For systemd, inspect `systemctl show zotonic -p LimitNOFILE -p TasksMax` and the unit's configured user and environment. If the process is approaching its descriptor limit, set a reviewed `LimitNOFILE=` in a service override, reload systemd, restart during a maintenance window and verify the new process limit.

Do not copy a universal file-limit number or size connection-tracking tables as a fixed fraction of memory. Check whether conntrack is on the traffic path, its observed occupancy, and the host's memory budget first. Aggressive timeout changes can break long-lived MQTT/WebSocket connections.

Change one setting at a time, record the previous value, load-test again, and keep a rollback procedure. A higher limit cannot repair a connection leak or an undersized database.
