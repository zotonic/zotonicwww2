---
name: "admin_mail_failures"
title: "Diagnose missing or failed email"
summary: "Trace one message from request to recipient."
category: "adminguide"
language: "en"
is_published: true
parent: "admin_collection_monitoring"
order: 4
required_modules: []
zotonic_keywords: ["troubleshooting", "operator", "email_delivery", "logging_and_monitoring"]
---

# Diagnose missing or failed email

**Access needed:** Mail-log access; provider access may be needed.

1. Identify one message by time, intended recipient, and originating workflow.
2. Check whether Zotonic created and queued it. If not, verify the workflow settings and logs.
3. Check whether a catch-all override or exception changed the destination.
4. Read the sending status and failure reason. Check relay authentication, TLS, connectivity, sender acceptance, and retry state as applicable.
5. If accepted by the provider, inspect its delivery or bounce record, then check the recipient's spam handling.
6. After correcting the cause, send one controlled test and confirm receipt before retrying a larger batch.

Do not resend an entire newsletter because one inbox is empty. Pending retries and provider acceptance can otherwise create duplicates. Record enough identifiers for diagnosis without copying private message content into a public ticket.
