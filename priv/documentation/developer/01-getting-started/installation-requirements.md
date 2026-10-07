---
name: "developer_installation_requirements"
title: "Installation requirements"
summary: "Check the required tools and install optional media tools when you need them."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_getting_started"
order: 10
required_modules: []
source_paths: ["docker-compose.yml", "docker/Dockerfile.dev", "docker/docker-entrypoint.sh", "shell.nix", "apps/zotonic_launcher/src/command/zotonic_cmd_addsite.erl"]
zotonic_keywords: ["reference", "backend_developer", "site_management", "configuration", "erlang_otp"]
---

# Installation requirements

For a first installation, follow [Set up a local development environment](local-environment.md). This page is the dependency checklist for a native build. The [container setup](install-containers.md) supplies its own dependencies.

## Required for this guide

| Tool | Purpose and check |
| --- | --- |
| Erlang/OTP 28 | Recommended runtime and compiler. Use the check below; `erl -version` reports a different version number. |
| GNU Make and a C/C++ toolchain | Build Zotonic and native dependencies. Check `make --version` and `c++ --version`. |
| Git | Fetch Zotonic and dependencies; check `git --version`. |
| PostgreSQL | Store CMS content. Use a supported release; the native walkthrough uses 16. Check `psql --version`, then test an actual connection. |
| ImageMagick | Process images. Check that `convert` and `identify` are available on Zotonic's command path. |
| gettext | Translation tooling; check `msgfmt --version`. |
| `fswatch` on macOS or `inotifywait` on Linux | Detect source edits during development. |

```sh
erl -noshell -eval 'io:format("~s~n", [erlang:system_info(otp_release)]), halt().'
```

Expect `28`. The guide's recommendation is distinct from the minimum accepted by a particular checkout. Use the requirements of that checkout when maintaining older releases.

A PostgreSQL client version does not prove a server is running. [Prepare local PostgreSQL](local-postgresql.md) checks the connection, role, and database. Password authentication is not PostgreSQL's `trust` method; do not replace authentication rules with `trust` to fix a failed password.

## Add tools when a feature needs them

Install FFmpeg for video processing, Ghostscript for applicable PDF/image conversion, and any additional tools required by the modules you enable. You do not need every optional service before making a first template edit.

## Platforms

Use the [macOS/Linux installation walkthrough](install-native.md), [Nix shell](install-nix.md), or [Docker/Podman](install-containers.md). On Windows, use a Linux environment such as WSL or the container route. FreeBSD users need GNU Make (`gmake`); adapt package and service commands to that platform rather than copying the Linux commands.

After checking the tools and database, continue with [Build and start Zotonic](build-start.md).
