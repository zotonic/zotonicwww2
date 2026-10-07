---
name: "developer_install_containers"
title: "Install with Docker or Podman"
summary: "Build and run Zotonic with its development containers, then create a site."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_getting_started"
order: 8
required_modules: []
source_paths: ["docker-compose.yml", "docker/Dockerfile.dev", "docker/docker-entrypoint.sh", "shell.nix", "apps/zotonic_launcher/src/command/zotonic_cmd_addsite.erl"]
zotonic_keywords: ["how_to_guide", "backend_developer", "site_management", "configuration", "erlang_otp"]
---

# Install with Docker or Podman

Use this route for a first try without installing Erlang or PostgreSQL on your computer. Start from the checkout created in [Set up a local development environment](local-environment.md).

## 1. Prepare the container runtime

Install [Docker with Compose](https://docs.docker.com/compose/install/) and start it. Check in your computer's terminal:

```sh
docker --version
docker compose version
```

If you already use Podman, install `podman-compose` as well and check `podman --version` and `podman-compose --version`. On macOS or Windows, start the runtime's Linux virtual machine before continuing. Windows users can work from a Linux checkout in WSL.

## 2. Build the development environment

From the Zotonic repository root, on your computer:

```sh
./start-docker.sh
```

With Podman, use `./start-podman.sh` instead. The script starts PostgreSQL, builds the development image, and compiles Zotonic on the first run. Wait for the build to finish and the container's shell prompt to appear. Downloads and the first compilation can take several minutes.

If host port 5432 is already used by a local database, run `DB_FORWARD_PORT=5433 ./start-docker.sh` (or the Podman script). Zotonic still connects to `postgres:5432` inside the container network. Ports 8000 and 8443 must also be available on your computer.

## 3. Start Zotonic

Run these commands **at the container shell prompt**:

```sh
bin/zotonic start
bin/zotonic status
```

Expect a running node and the status site's state. Open [the local status site](https://localhost:8443/) in your computer's browser. Zotonic generates a self-signed development certificate; the browser may ask you to accept it for this local address.

The container already supplies the database connection. Do not run the native PostgreSQL setup or change its database host to `localhost`.

## 4. Create your first site

Keep this container shell open and continue with [Create your first site](create-site.md). Edit the generated files with your normal editor on your computer: the checkout is mounted into the container.

## Stop and return later

At the container shell, run `bin/zotonic stop`, then `exit`. To stop the database container too, run `docker compose stop postgres` on your computer (or `podman-compose stop postgres`). Start the script again and run `bin/zotonic start` when you return; your files and database remain.

Site files live in `apps_user/`, configuration and site files in `docker-data/`, and PostgreSQL data in the Compose `pgdata` volume. Do not remove that volume to solve a routine startup error. This supplied setup uses development credentials and exposed ports; use it on a trusted development machine. Use the deployment guide when preparing a public server.
