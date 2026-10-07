---
name: "developer_install_native"
title: "Install on macOS or Linux"
summary: "Install the native tools, prepare PostgreSQL, and build a local checkout."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_getting_started"
order: 9
required_modules: []
source_paths: ["docker-compose.yml", "docker/Dockerfile.dev", "docker/docker-entrypoint.sh", "shell.nix", "apps/zotonic_launcher/src/command/zotonic_cmd_addsite.erl"]
zotonic_keywords: ["how_to_guide", "backend_developer", "site_management", "configuration", "erlang_otp", "postgresql"]
---

# Install on macOS or Linux

Use this route to run Zotonic directly on your computer. Start with the checkout in [Set up a local development environment](local-environment.md). If you just want to try Zotonic, the [container route](install-containers.md) supplies these dependencies for you.

## 1. Install the tools

### macOS with Homebrew

Install Apple's command-line build tools if they are missing:

```sh
xcode-select --install
```

With [Homebrew](https://brew.sh/) installed, run:

```sh
brew install erlang@28 postgresql@16 git imagemagick gettext fswatch
export PATH="$(brew --prefix erlang@28)/bin:$(brew --prefix postgresql@16)/bin:$(brew --prefix gettext)/bin:$PATH"
brew services start postgresql@16
```

The versioned Erlang and PostgreSQL packages need the `PATH` line above. Add that line to your shell startup file to keep it in new terminals. These package names are documented by [Homebrew for OTP 28](https://formulae.brew.sh/formula/erlang@28) and [PostgreSQL 16](https://formulae.brew.sh/formula/postgresql@16).

### Ubuntu or Debian

Install the system tools and start PostgreSQL:

```sh
sudo apt-get update
sudo apt-get install build-essential git postgresql postgresql-client imagemagick gettext inotify-tools libseccomp-dev
sudo systemctl start postgresql
```

Install **Erlang/OTP 28** using an [OTP 28 installation option](https://www.erlang.org/downloads/28). Distribution packages differ: do not assume `apt-get install erlang` selects OTP 28. If you do not already have a suitable Erlang package or version manager, the [Nix shell](install-nix.md) or [container route](install-containers.md) avoids that extra setup.

## 2. Check the tools

```sh
erl -noshell -eval 'io:format("~s~n", [erlang:system_info(otp_release)]), halt().'
psql --version
git --version
make --version
```

The Erlang check should print `28`. `make` should be GNU Make. Use a supported PostgreSQL release; this walkthrough uses PostgreSQL 16. If a command is missing, fix the installation or shell path before continuing. See [Installation requirements](installation-requirements.md) for optional media tools.

## 3. Prepare the database

Follow [Prepare local PostgreSQL](local-postgresql.md). It creates the development role and database and checks a connection using the same host and credentials Zotonic will use.

## 4. Build and start

Return to the Zotonic checkout and follow [Build and start Zotonic](build-start.md), then [Create your first site](create-site.md). Run these commands as your normal user, not with `sudo`.
