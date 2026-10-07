---
name: "developer_install_nix"
title: "Use the Nix development shell"
summary: "Enter the supplied OTP 28 development shell, prepare PostgreSQL, and build."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_getting_started"
order: 12
required_modules: []
source_paths: ["docker-compose.yml", "docker/Dockerfile.dev", "docker/docker-entrypoint.sh", "shell.nix", "apps/zotonic_launcher/src/command/zotonic_cmd_addsite.erl"]
zotonic_keywords: ["how_to_guide", "backend_developer", "site_management", "configuration", "erlang_otp", "postgresql"]
---

# Use the Nix development shell

Use this route if you already work with Nix. Start from the checkout in [Set up a local development environment](local-environment.md). Install Nix using its [official installation instructions](https://nixos.org/download/) if needed.

## 1. Enter the supplied shell

From the repository root:

```sh
nix-shell
```

Wait for dependencies to finish downloading or building. The checked-in `shell.nix` selects `erlang_28` and includes PostgreSQL tools and the platform's file watcher. Keep this shell open for all build and Zotonic commands.

Check the runtime:

```sh
erl -noshell -eval 'io:format("~s~n", [erlang:system_info(otp_release)]), halt().'
```

Expect `28`.

## 2. Start a local database

The shell supplies PostgreSQL programs; it does not start a database server. If you already have a local server, follow [Prepare local PostgreSQL](local-postgresql.md).

Otherwise, create a new development cluster in the checkout, as your normal user:

```sh
initdb -D .local-postgres --auth-local=peer --auth-host=scram-sha-256
pg_ctl -D .local-postgres -l .local-postgres/server.log -o "-h localhost -p 5432 -k $(pwd)/.local-postgres" start
```

Add `.local-postgres/` to `.git/info/exclude` so database files stay out of Git. Use an empty directory for `initdb`. If port 5432 is already occupied, use the existing local server or configure a different port throughout.

Local peer authentication uses your operating-system username. Open `psql -h "$(pwd)/.local-postgres" postgres` as that same user and follow the SQL in [Prepare local PostgreSQL](local-postgresql.md).

## 3. Build and try a site

Follow [Build and start Zotonic](build-start.md) and [Create your first site](create-site.md) inside the Nix shell. If you open another terminal for Zotonic commands, enter `nix-shell` there too.

When finished, stop Zotonic with `bin/zotonic stop`. If you started the cluster above, stop it with `pg_ctl -D .local-postgres stop` before leaving the shell. Keep the directory to reuse its data next time.
