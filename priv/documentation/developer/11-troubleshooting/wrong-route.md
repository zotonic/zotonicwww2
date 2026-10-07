---
name: "developer_wrong_route"
title: "A URL reaches the wrong page"
summary: "Confirm the hostname, site, and path from the actual request. Use the dispatch trace to see the selected rule and any rewritten path."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_troubleshooting"
order: 5
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command", "apps/zotonic_mod_development", "apps/zotonic_filehandler/src"]
zotonic_keywords: ["troubleshooting", "backend_developer", "routing_and_redirects", "dispatch_rule"]
---

# A URL reaches the wrong page

Confirm the hostname, site, and path from the actual request. Use the dispatch trace to see the selected rule and any rewritten path.

Check route order and arguments, then inspect resource page paths and language handling if the URL is content-driven. A path tested against the wrong site can produce a convincing but irrelevant trace.

After changing a rule or resource path, refresh the relevant information and test the browser request, including its redirects. Verify another nearby URL to catch an overly broad match.
