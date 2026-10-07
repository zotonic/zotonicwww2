---
name: "admin_incident"
title: "Hand over an incident and record recovery"
summary: "Give the next operator enough context to continue safely."
category: "adminguide"
language: "en"
is_published: true
parent: "admin_collection_monitoring"
order: 6
required_modules: []
zotonic_keywords: ["troubleshooting", "operator", "logging_and_monitoring", "reliability"]
---

# Hand over an incident and record recovery

**Access needed:** Operator responsible for the incident.

1. State the affected site and environment, when the problem started, and which users or operations are affected.
2. Record the last known working revision and recent changes.
3. List checks performed, their results, and any temporary configuration or paused jobs.
4. Attach relevant redacted errors and identifiers, with links to controlled logs rather than copied secrets.
5. Name the current owner, the next action, and the condition for escalation or recovery.
6. After service returns, verify the original workflow, remove temporary changes, and record the recovery point and outstanding work.

Keep a timeline so another operator does not repeat a destructive or already failed action. Do not label an incident resolved merely because a process restarted; confirm the user-facing operation and delayed background work.
