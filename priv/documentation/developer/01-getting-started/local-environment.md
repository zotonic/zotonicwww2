---
name: "developer_local_environment"
title: "Set up a local development environment"
summary: "Choose one installation route, start Zotonic, create a site, and make your first visible change."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_getting_started"
order: 1
required_modules: []
source_paths: ["rebar.config", "GNUmakefile", "apps/zotonic_mod_zotonic_site_management/priv/skel", "apps/zotonic_launcher/src/command/zotonic_cmd_addsite.erl"]
zotonic_keywords: ["tutorial", "backend_developer", "site_management", "development_and_debugging", "erlang_otp"]
---

# Set up a local development environment

Start here to build Zotonic, open your own site, and change a page. You do not need to learn Erlang before trying the first template edit.

## 1. Choose an installation route

::: aside
The [installation requirements](installation-requirements.md) explain the tools and version checks. They are a checklist to consult, not another required tutorial.
:::

**For a first try, use [Install with Docker or Podman](install-containers.md).** It supplies Erlang/OTP 28, build tools, and PostgreSQL. You only install Git and a container runtime on your computer.

Prefer running everything directly on your computer? Follow [Install on macOS or Linux](install-native.md). Already use Nix? Follow [Use the Nix development shell](install-nix.md). Choose one route; you do not need all three.

## 2. Get the source

In a terminal, move to the directory where you keep projects, then run:

```sh
git clone https://github.com/zotonic/zotonic.git
cd zotonic
```

This gets the development branch used by this guide. If you need a released version, get its source from the [Zotonic releases](https://github.com/zotonic/zotonic/releases) and follow instructions for that release. Do not mix an existing native build with a container build; use separate checkouts.

## 3. Install and start

Open your chosen installation page above and follow it until `bin/zotonic status` reports a running node. Container users run Zotonic commands **inside the container shell**; native users run them in this checkout.

## 4. Make it yours

[Create your first site](create-site.md), log in to its admin, then [change its home-page template](first-template-change.md). The example is a small blog named `garden`.

You are done with the first run when the site opens in your browser and your own text appears after a template edit. Continue with [your first dynamic page](first-dynamic-page.md) when you want to add a new URL.
