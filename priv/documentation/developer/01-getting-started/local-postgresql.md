---
name: "developer_local_postgresql"
title: "Prepare local PostgreSQL"
summary: "Create and check the database used by a native development installation."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_getting_started"
order: 11
required_modules: []
source_paths: ["docker-compose.yml", "docker/Dockerfile.dev", "docker/docker-entrypoint.sh", "shell.nix", "apps/zotonic_launcher/src/command/zotonic_cmd_addsite.erl"]
zotonic_keywords: ["how_to_guide", "backend_developer", "database", "postgresql", "configuration"]
---

# Prepare local PostgreSQL

Use this page for the native installation or a Nix development shell. The [container route](install-containers.md) already configures PostgreSQL; skip this page for containers.

## 1. Open a database administrator session

On Ubuntu/Debian with the PostgreSQL service running:

```sh
sudo -u postgres psql
```

On macOS with a fresh Homebrew PostgreSQL installation:

```sh
psql postgres
```

An existing installation may use a different administrator account. Use that account rather than resetting an existing database.

## 2. Create a local development role and database

At the `psql` prompt:

```sql
CREATE ROLE zotonic LOGIN;
\password zotonic
CREATE DATABASE zotonic OWNER zotonic ENCODING 'UTF8' TEMPLATE template0;
\q
```

For this disposable local example, enter `zotonic` at both password prompts. This matches Zotonic's development defaults. Use it only on your local development machine. If the role or database already exists, inspect its owner and settings instead of recreating it or changing its password blindly.

The `garden` site will use its own schema inside this database. The database owner can create that schema; the application role does not need superuser privileges.

## 3. Test the same connection Zotonic will use

```sh
psql -h localhost -U zotonic -d zotonic -W -c 'SELECT current_user, current_database();'
psql -h localhost -U zotonic -d postgres -W -c 'SELECT current_database();'
```

Enter the development password. Expect `zotonic` for both values in the first query and `postgres` in the second. `addsite` also connects to the `postgres` maintenance database to check that the site database exists. Do not continue to `addsite` until this succeeds.

If the server refuses the connection, check that PostgreSQL is running on localhost port 5432. If authentication fails, check the password and the first matching `pg_hba.conf` rule. From the administrator's `psql` session, `SHOW hba_file;` locates that file. A local password-based configuration can use:

```text
host    zotonic,postgres    zotonic    127.0.0.1/32    scram-sha-256
host    zotonic,postgres    zotonic    ::1/128         scram-sha-256
```

Place specific rules before a broader conflicting rule, then reload PostgreSQL with `SELECT pg_reload_conf();` in the administrator session. [PostgreSQL's authentication documentation](https://www.postgresql.org/docs/16/auth-pg-hba-conf.html) explains that only the first matching rule applies.

## Optional: use a local Unix socket

If Zotonic and PostgreSQL run on the same host, you can use a Unix socket instead of TCP. In Zotonic's database configuration, set `{dbhost, socket}` for `/run/postgresql`, or `{dbhost, "/var/run/postgresql"}` for a different absolute socket directory. The configured `dbport` selects the socket filename `.s.PGSQL.<port>`; it is normally 5432. Use the actual directory reported by `SHOW unix_socket_directories;`.

For example, in the site's configuration term list:

```erlang
{dbhost, socket},
{dbport, 5432}
```

Test the same endpoint and role:

```sh
psql -h /run/postgresql -p 5432 -U zotonic -d zotonic -W
```

Socket connections match `local` rules in `pg_hba.conf`. For password authentication, a specific rule is `local zotonic,postgres zotonic scram-sha-256`. Peer authentication instead checks the operating-system identity: test as the service user and configure a deliberate user mapping when the names differ. Socket mode does not silently fall back to TCP. The service must be able to access the socket directory; a container needs an explicitly shared socket path.

When creating a site, use `bin/zotonic addsite -h socket garden` (or the absolute directory after `-h`) together with any other required database options.

## 4. Continue the installation

[Build and start Zotonic](build-start.md), then create `garden`. For a different database host, role, or password, configure those values before creating the site; see [the addsite options](../12-command-reference/cmd-addsite.md). Container connections use host `postgres`, not `localhost`.
