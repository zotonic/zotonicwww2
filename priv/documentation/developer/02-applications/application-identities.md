---
name: "developer_application_identities"
title: "Erlang applications, Zotonic modules, and sites"
summary: "These terms describe different parts of the system."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_applications"
order: 2
required_modules: []
source_paths: ["rebar.config", "apps/zotonic_core/src/support/z_module_manager.erl", "apps/zotonic_core/src/support/z_module_indexer.erl", "apps/zotonic_mod_zotonic_site_management/priv/skel"]
zotonic_keywords: ["explanation", "backend_developer", "development_and_debugging", "module", "site"]
---

# Erlang applications, Zotonic modules, and sites

These terms describe different parts of the system.

::: aside
Putting a package on the code path is different from activating its Zotonic module for a particular site. Use [Application startup and per-site module activation](startup-activation.md) to understand this distinction when a compiled feature is missing.
:::

An **Erlang application** is a package described by a `.app` file, normally generated from `src/<name>.app.src`. It can provide modules and an OTP application callback.

A **Zotonic module** is functionality that can be activated for a site. Its main Erlang module declares attributes such as `-mod_title` and `-mod_depends`.

A **site** is a named website with its own configuration and runtime context. Several sites share the same Erlang VM and loaded code, while module activation and site data are site-specific.

