---
name: "developer_datamodel_fixtures"
title: "Install initial content with a datamodel"
summary: "Use a datamodel for application-owned categories, predicates, resources, and relationships that must exist when a module is installed."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_content_data"
order: 8
required_modules: []
source_paths: ["apps/zotonic_core/src/models", "apps/zotonic_core/src/support/z_datamodel.erl", "apps/zotonic_core/src/support/z_search.erl"]
zotonic_keywords: ["how_to_guide", "backend_developer", "content_modeling", "resource", "module"]
---

# Install initial content with a datamodel

Use a datamodel for resources that a module needs on installation. Add this declaration and callback to the `mod_garden.erl` from the module-creation task:

```erlang
-include_lib("zotonic_core/include/zotonic.hrl").
-mod_schema(1).
-export([manage_schema/2]).

-spec manage_schema(Version, Context) -> Datamodel
    when Version :: install | {upgrade, pos_integer()},
         Context :: z:context(),
         Datamodel :: #datamodel{}.
manage_schema(install, _Context) ->
    #datamodel{
        resources = [
            {garden_welcome, text, #{
                <<"title">> => <<"Welcome to the garden">>,
                <<"is_published">> => false
            }}
        ]
    }.
```

Put declarations before functions and include the header only once. Compile and activate the module on a fresh local database-backed site. The module manager applies the returned datamodel; do not call the callback manually expecting it to create data. Confirm `m_rsc:rid(garden_welcome, C)` returns an ID and find the unpublished page in the admin.

A module already installed with a schema version needs an explicit upgrade step for later changes. Give managed resources stable names, and decide which properties editors own. Test with an edited title before applying an upgrade: returning defaults must not be treated as permission to replace editorial work.
