---
name: "admin_logs"
title: "Read logs and collect useful evidence"
summary: "Find the relevant failure without exposing credentials or personal data."
category: "adminguide"
language: "en"
is_published: true
parent: "admin_collection_monitoring"
order: 2
required_modules: []
zotonic_keywords: ["troubleshooting", "operator", "logging_and_monitoring"]
---

# Read logs and collect useful evidence

**Access needed:** Site log permission or server log access.

::: aside
Container and host service logs may be stored in different places.
:::

1. Record the time, environment, hostname, affected page, and action that failed.
2. Check the relevant site log or service log around that time.
3. Identify the first relevant error and its module, request or correlation identifier, and repeated causes.
4. Compare with the last deployment or configuration change.
5. Reproduce once with safe test data when possible and note the expected and actual result.
6. Share a short redacted excerpt and the reproduction steps with the responsible operator or developer.

Do not publish complete configuration dumps, cookies, authorization headers, password-reset URLs, or message bodies. Retain detailed logs in the controlled incident record.

More logging is not always better: temporary debug logging can expose data or fill disks. If enabled for diagnosis, record the change and turn it off afterwards.
