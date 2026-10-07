# Run Zotonic as a service

Use one service manager for each installation. On a Linux host with systemd, this is a starting point for `/etc/systemd/system/zotonic.service`:

```ini
[Unit]
Description=Zotonic
After=network.target

[Service]
Type=simple
User=zotonic
Group=zotonic
WorkingDirectory=/srv/zotonic
ExecStart=/srv/zotonic/bin/zotonic start_nodaemon
Restart=on-failure
RestartSec=5
TimeoutStopSec=120

[Install]
WantedBy=multi-user.target
```

Adapt the account and paths to the installation. Ensure Erlang/OTP 28 and media tools are on the service's PATH, and that configuration and persistent storage are readable or writable by this account as appropriate. The process must run in the foreground; do not put `bin/zotonic start` behind `Type=simple`.

After reviewing the unit, run `systemctl daemon-reload`, enable/start it through your operating procedure, and inspect `systemctl status zotonic` and `journalctl -u zotonic`. Check a public page, login and an uploaded file. Test a restart and a host reboot in acceptance before production.

Keep secrets in the deployment's protected configuration. Containers should use their platform's restart and storage policies instead of a second systemd process inside the container. The old distribution-specific init scripts are historical examples, not the default installation route.
