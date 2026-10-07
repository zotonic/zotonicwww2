---
name: "developer_build_start"
title: "Build and start Zotonic"
summary: "Compile Zotonic, start it, check the status page, and continue to your first site."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_getting_started"
order: 2
required_modules: []
source_paths: ["rebar.config", "GNUmakefile", "apps/zotonic_mod_zotonic_site_management/priv/skel", "apps/zotonic_launcher/src/command/zotonic_cmd_addsite.erl"]
zotonic_keywords: ["how_to_guide", "backend_developer", "development_and_debugging", "site_management"]
---

# Build and start Zotonic

Complete one [installation route](local-environment.md) first. For a native or Nix setup, the tools and [database connection](local-postgresql.md) should now be ready. Container users whose node is already running can go straight to [Create your first site](create-site.md).

## 1. Build

From the Zotonic repository root, as your normal user:

```sh
make
```

The first build downloads dependencies and compiles Zotonic. Wait for the successful-compilation message. If it fails, resolve the first build error before continuing; a missing compiler or wrong Erlang version is a setup issue.

## 2. Start

For the first walkthrough, start in the background so the same terminal remains available:

```sh
bin/zotonic start
bin/zotonic status
```

Expect a running node and sites with their states. Open [the local status site](https://localhost:8443/). This is the installation's management page, not your new website. The initial HTTPS certificate is self-signed; accept it only for this local development address.

If the node is already running, use `status` rather than starting a second instance. If startup fails, check whether ports 8000/8443 are occupied and read the reported log location.

## 3. Create a site

Continue with [Create your first site](create-site.md). Keep using this repository root for `bin/zotonic` commands.

## Foreground development and stopping

For startup output and an interactive Erlang shell, use `bin/zotonic debug` instead of `start`, with Zotonic stopped first. Run further OS commands from a second terminal in the same environment. Do not type shell commands such as `bin/zotonic status` at the Erlang prompt.

Use `bin/zotonic stop` to stop this installation. Attach to an existing node with `bin/zotonic shell`; Ctrl-C twice detaches that remote shell without stopping the node. See [startup modes](../03-command-line-workflows/startup-modes.md) for details.
