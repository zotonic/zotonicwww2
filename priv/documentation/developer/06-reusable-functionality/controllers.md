---
name: "developer_controllers"
title: "Add an HTTP controller"
summary: "Use controller_template when a dispatch rule only needs to render a template. Write a custom controller when the endpoint needs additional request processing or response behavior."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_reusable_functionality"
order: 4
required_modules: []
source_paths: ["apps/zotonic_core/src/behaviours", "apps/zotonic_mod_base/src", "apps/zotonic_core/include/zotonic_notifications.hrl"]
zotonic_keywords: ["how_to_guide", "backend_developer", "controller", "http"]
---

# Add an HTTP controller

Use `controller_template` for a page that only renders a template. For a public, read-only plain-text endpoint, create `src/controllers/controller_garden_hello.erl` in the active Garden application:

```erlang
-module(controller_garden_hello).
-export([content_types_provided/1, process/4]).

-spec content_types_provided(Context) -> {Types, Context}
    when Context :: z:context(), Types :: list().
content_types_provided(Context) ->
    {[{<<"text">>, <<"plain">>, []}], Context}.

-spec process(Method, Accepted, Provided, Context) -> {Body, Context}
    when Method :: binary(), Accepted :: term(), Provided :: term(),
         Context :: z:context(), Body :: binary().
process(_Method, _Accepted, _Provided, Context) ->
    {<<"Welcome to the garden">>, Context}.
```

Add a rule to the application's `priv/dispatch/garden` list:

```erlang
{garden_hello, ["garden-hello"], controller_garden_hello, []}
```

Compile, refresh discovery, and request `/garden-hello` on the site's hostname. Expect a plain-text response containing the greeting. Check dispatch if it returns another page. The rule above is one tuple inside the dispatch list, not an entire dispatch file.

This example deliberately returns public constant text. Private data needs explicit authorization callbacks and model checks; a write endpoint also needs method handling, input validation, and protection against unauthorized requests. Consult `controller#controller_template` for a built-in alternative and `controller#controller_api` for model-based HTTP access.
