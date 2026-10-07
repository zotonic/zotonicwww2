---
name: "admin_health_checks"
title: "Check that a site is working"
summary: "Monitor the user-facing service as well as its processes."
category: "adminguide"
language: "en"
is_published: true
parent: "admin_collection_monitoring"
order: 1
required_modules: []
zotonic_keywords: ["how_to_guide", "operator", "logging_and_monitoring", "reliability", "monitor"]
---

# Check that a site is working

**Access needed:** Monitoring access; server access for node checks.

1. From outside the server, request a known public page and check its status and expected content.
2. Check certificate validity, redirects, and response time for the public hostname.
3. On the server, use `bin/zotonic status` to verify the intended node and site states.
4. Check error logs, disk capacity, database availability, and backup age.
5. Use a dedicated account for a safe representative authenticated check when required.
6. Monitor important asynchronous work, such as email and media processing, separately.

Alert on user-visible failure, approaching capacity limits, stale backups, and repeated processing failures. Set thresholds from normal operation and name an owner for each alert; do not create alerts nobody can act on.

A successful HTTP response from a fallback or status page can hide a failed site. Match expected content or a site-specific health response, not just status 200. Keep monitoring credentials out of public check URLs.
