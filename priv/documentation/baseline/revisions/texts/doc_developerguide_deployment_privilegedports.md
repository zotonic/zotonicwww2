# Serve HTTPS on a privileged port

Run Zotonic as an unprivileged service account on its configured high ports. Let the host's reverse proxy accept public HTTP/HTTPS and forward to a loopback or otherwise private backend. Verify that the backend is not accidentally reachable from the internet.

Configure the public hostname and advertised ports separately from listener ports. Trust forwarded headers only from the actual proxy addresses. Test redirect destinations, secure cookies, large uploads and MQTT/WebSocket connections.

If your deployment deliberately gives Zotonic direct access to a low port, use the operating system's narrowly scoped service capability mechanism and test it with the installed runtime. Avoid running the whole application as root or adding capabilities to a hard-coded `beam.smp` path that changes on every Erlang upgrade.

Do not copy interface-name parsing or firewall-redirection commands from old distribution recipes. IPv4, IPv6, containers and existing firewall policy all affect the result.
