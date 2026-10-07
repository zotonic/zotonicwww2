---
name: "developer_create_site"
title: "Create your first site"
summary: "Create the garden blog, open it in your browser, and sign in to its admin."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_getting_started"
order: 3
required_modules: []
source_paths: ["rebar.config", "GNUmakefile", "apps/zotonic_mod_zotonic_site_management/priv/skel", "apps/zotonic_launcher/src/command/zotonic_cmd_addsite.erl"]
zotonic_keywords: ["how_to_guide", "backend_developer", "development_and_debugging", "site_management"]
---

# Create your first site

Start with a running Zotonic node from [your installation route](local-environment.md). Run the commands below from the repository root, inside the container or Nix shell if you chose one.

## 1. Create the example site

```sh
bin/zotonic addsite -s blog -H garden.test garden
bin/zotonic status
```

The command creates `apps_user/garden`, prepares the database schema, compiles the application, and starts it on the running node. Expect `garden` to reach the running state. Keep the generated admin password shown in the output; you need it for the next step.

## 2. Give your browser the local hostname

On the computer running your browser, add this line to its hosts file using an administrator-enabled text editor:

```text
127.0.0.1 garden.test
```

The file is `/etc/hosts` on macOS/Linux and `C:\Windows\System32\drivers\etc\hosts` on Windows. For containers, edit the computer's hosts file, not just the container's. Keep any existing entries. You can skip this step if your local DNS already resolves `garden.test` to this machine.

## 3. Open the site and admin

Open [your garden site](https://garden.test:8443/) in the browser. These examples use the default development HTTPS port 8443; use your configured port if different. Expect the blog home page. The generated development certificate may need a local browser exception.

Open [the garden admin](https://garden.test:8443/admin) and log in as **admin**, using the password printed by `addsite`. These are your site's credentials, separate from the status site's `wwwadmin` account.

## 4. Make your first change

Open `apps_user/garden` in your editor, then follow [Change a template and see the result](first-template-change.md). There is no need to create a custom module first.

If the wrong site opens, check the hosts entry and `priv/zotonic_site.config` hostname. If creation fails, read the error and inspect the partially created directory before retrying. Native database problems can be checked with [Prepare local PostgreSQL](local-postgresql.md); containers use database host `postgres`. See [addsite options](../12-command-reference/cmd-addsite.md) for a non-default configuration.
