---
name: "developer_cmd_addsite"
title: "addsite: Create a site application from a skeleton"
summary: "Create a site application from a skeleton."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_reference"
order: 1
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command/zotonic_cmd_addsite.erl"]
zotonic_keywords: ["reference", "backend_developer", "site_management", "create"]
---

# addsite: Create a site application from a skeleton

Create a site application from a skeleton.

## Syntax

```text
bin/zotonic addsite [options] <site_name>
```

## Requirements and effects

Creates files and prepares site data; it then attempts to compile and start the site through the running node.

Use `-s blog`, `-s empty`, or `-s nodb` to select a skeleton; the default is blog. Use `-H hostname` for its host. `-L` creates in the current directory and links into apps_user. `-G url` clones a repository first. Database options are `-h` host, `-p` port, `-u` user, `-P` password, `-d` database, and `-n` schema. `-a` sets the admin password. `-A true` adds application startup and a supervisor; `-U true` creates a multi-app layout. Treat generated credentials as private. If startup fails, inspect the created files before retrying.

## Example

```sh
bin/zotonic addsite -s blog -H garden.test garden
```

Run commands from the Zotonic workspace root. Replace `garden` with your site name where applicable.
