# Installation review — 7 October 2026

The previous onboarding page assumed the reader could install dependencies,
configure PostgreSQL, choose a hostname, and obtain the source without showing
those steps. Its README link led back to the old installation documentation.
The baseline pages duplicated platform instructions, recommended different OTP
versions, described password authentication as “trust”, and mixed local setup
with provisioning a public server.

## Revised learning path

1. Local environment: choose one route and clone the source.
2. Containers (suggested first try), native macOS/Linux, or an existing Nix setup.
3. Start and check Zotonic; native/Nix instructions include PostgreSQL setup.
4. Create `garden`, resolve its hostname on the browser's computer, and log in.
5. Edit the known `home.tpl` content block and see the result.

The requirements checklist supports this path instead of interrupting it. The
pages distinguish host commands, container commands, SQL, and the Erlang shell.
Each route names its next task. Related tasks remain graph connections; inline
links identify the steps necessary at a particular point in the walkthrough.

The [baseline revision map](../baseline/revisions/installation.json) gives three
existing pages complete replacement texts without changing the original exports.
Native setup, Nix, PostgreSQL, and build/start remain separate task resources.
The integration maps preserve the old resource identities and URLs.

## Verification

Checked commands and defaults against `start-docker.sh`, `start-podman.sh`,
`docker-compose.yml`, `docker/Dockerfile.dev`, `docker/docker-entrypoint.sh`,
`docker/zotonic-docker.config`, `shell.nix`, `GNUmakefile`, `z_config`,
`zotonic_cmd_addsite`, `zotonic_status_addsite`, `z_db`, and the blog skeleton.
In particular, `addsite` also connects to the `postgres` maintenance database;
the native connection checks and authentication example cover it.

Checked current upstream instructions for [OTP 28](https://www.erlang.org/downloads/28),
[Homebrew Erlang](https://formulae.brew.sh/formula/erlang@28),
[Homebrew PostgreSQL](https://formulae.brew.sh/formula/postgresql@16),
[Docker Compose](https://docs.docker.com/compose/install/),
[Nix installation](https://nixos.org/download/), and PostgreSQL's
[authentication rules](https://www.postgresql.org/docs/16/auth-pg-hba-conf.html),
[initdb](https://www.postgresql.org/docs/16/app-initdb.html), and
[pg_ctl](https://www.postgresql.org/docs/16/app-pg-ctl.html).

The local environment has neither Docker nor Nix on its command path. No fresh
container, Nix, or native installation was executed, and no existing database or
running site was changed for this review. Source checks and successful Markdown
rendering do not establish a clean-machine installation test. Before publication,
run the full walkthrough in clean Docker and native environments, including
site creation, browser login, the template edit, and stop/restart persistence.

Validation completed: both bundles rendered (160 developer resources and 119
editor resources); all five connection tests passed; 18 getting-started shell
examples passed `bash -n`; all 96 baseline hashes matched; the three replacement
mappings agreed with both integration CSVs without duplicate target ownership.
The review inventory contains every developer resource exactly once.
