---
name: "admin_hostnames_https"
title: "Configure hostnames and HTTPS"
summary: "Align DNS, site selection, and certificate termination."
category: "adminguide"
language: "en"
is_published: true
parent: "admin_collection_services"
order: 2
required_modules: []
zotonic_keywords: ["how_to_guide", "operator", "routing_and_redirects", "tls_and_certificates", "configuration"]
---

# Configure hostnames and HTTPS

**Access needed:** DNS and server configuration access.

1. Decide the canonical hostname and required aliases. Make DNS point to the intended server or reverse proxy.
2. Set the matching site hostname and aliases, and check the externally visible HTTP/HTTPS ports.
3. Choose where TLS terminates: Zotonic or a reverse proxy. Install certificates through that component's supported mechanism.
4. If using a proxy, forward the expected host and protocol information and support WebSocket connections. Keep direct backend access consistent with the deployment's access policy.
5. Test the canonical URL and each alias, redirects, certificate hostname and chain, admin login, and a live browser interaction.
6. Check certificate renewal and monitor expiry before it becomes an outage.

A working DNS record does not select the correct Zotonic site unless the hostname matches its configuration. A certificate working in your own browser is not proof that public clients trust it.

For a local first run, use the developer setup instructions. Production certificates, public redirects, and proxy settings need the actual deployment's reviewed configuration.
