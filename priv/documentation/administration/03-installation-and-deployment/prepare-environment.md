---
name: "admin_prepare_environment"
title: "Prepare an acceptance or production environment"
summary: "Separate environments and record how the site is operated."
category: "adminguide"
language: "en"
is_published: true
parent: "admin_collection_deployment"
order: 2
required_modules: []
zotonic_keywords: ["how_to_guide", "operator", "site_management", "configuration", "reliability"]
---

# Prepare an acceptance or production environment

**Access needed:** Server administrator and deployment access.

Use the local installation walkthrough for learning. Before running a shared acceptance or production site, agree its hostname, data location, service owner, and recovery arrangements.

1. Select a code revision and use Erlang/OTP 28 for this documentation's deployment path.
2. Create separate database credentials, storage, and configuration for each environment.
3. Set the environment deliberately and confirm the effective configuration. Keep development passwords and debug interfaces out of the public setup.
4. Configure outbound email and other integrations for that environment. On acceptance, direct test mail to a controlled recipient before exercising workflows.
5. Establish service startup, HTTPS, backups, and external health checks.
6. Record how to deploy, stop, restart, and recover the installation, including who has access.

If copying production data to acceptance, restrict access and handle its personal data appropriately. Separate database names alone do not isolate email, external file stores, queues, or scheduled integrations.
