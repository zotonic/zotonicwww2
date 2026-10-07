---
name: "admin_database_connection"
title: "Check or change a site database connection"
summary: "Verify host, role, database, and schema before restarting a site."
category: "adminguide"
language: "en"
is_published: true
parent: "admin_collection_services"
order: 6
required_modules: []
zotonic_keywords: ["how_to_guide", "operator", "database", "postgresql", "configuration"]
---

# Check or change a site database connection

**Access needed:** Database and server configuration access.

1. Identify the site's effective database host, port, database name, role, and schema. Do not print the password into a shared report.
2. Test connectivity from the same host or container that runs Zotonic. A successful connection from your laptop may follow a different route.
3. Check authentication and the role's access to the intended database and schema.
4. Make a required connection change in acceptance first, at the owning configuration layer.
5. Restart or reload according to the deployment procedure and check startup logs.
6. Verify an existing page and a small write using the intended site account. Confirm that you did not connect to an empty or wrong environment's database.

A schema separates site tables within a database; it is not a substitute for environment isolation. Do not grant superuser rights to bypass a permissions error. For a fresh local development database, use the developer setup task instead of adapting production credentials.

## Local socket connections

For PostgreSQL on the same host, set `dbhost` to `socket` (atom, string, or binary) to use `/run/postgresql`, or set it to an absolute socket directory. `dbport` selects `.s.PGSQL.<port>`, normally `.s.PGSQL.5432`. This mode does not fall back to TCP.

Check `SHOW unix_socket_directories;` in PostgreSQL and test with `psql -h /run/postgresql -p 5432 -U ROLE -d DATABASE` as the Zotonic service user. Socket connections use `local` rules in `pg_hba.conf`; TCP connections use `host` rules. Match the intended password or peer-authentication policy. Keep the socket directory writable only by trusted users. For containers, verify the mount and permissions inside the container.
