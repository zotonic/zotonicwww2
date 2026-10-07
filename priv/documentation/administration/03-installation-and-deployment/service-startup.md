---
name: "admin_service_startup"
title: "Configure service startup and restart behaviour"
summary: "Run Zotonic under the installation’s service manager."
category: "adminguide"
language: "en"
is_published: true
parent: "admin_collection_deployment"
order: 4
required_modules: []
zotonic_keywords: ["how_to_guide", "operator", "site_management", "configuration", "reliability"]
---

# Configure service startup and restart behaviour

**Access needed:** Server administrator.

Choose one owner for the Zotonic process: the host's service manager or the container platform. Agree the service user, working directory, configuration paths, persistent storage, and log destination.

1. Use the installation's reviewed service definition and ensure it runs Zotonic as the intended unprivileged account.
2. Verify that Erlang/OTP 28 and required external programs are on that service's path, not only your interactive shell's path.
3. Check database availability, filesystem permissions, restart policy, and shutdown timeout.
4. Start through the service manager and check both its state and `bin/zotonic status`.
5. Restart in acceptance and verify the public page, login, and an uploaded file survive.
6. Test startup after a host restart in acceptance before relying on automatic recovery.

Do not simultaneously start a foreground debug node using the same ports and node name. Keep service-specific commands in the installation's operating notes: systemd, containers, and hosted platforms differ. The older Cloud-Init and init-script examples need platform review before reuse.
