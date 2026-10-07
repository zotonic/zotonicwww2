---
name: "developer_testing_strategy"
title: "Choose checks for a change"
summary: "Choose checks for a change."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_testing"
order: 1
required_modules: []
source_paths: ["apps/zotonic_core/test", "apps/zotonic_launcher/src/command/zotonic_cmd_runtests.erl", "apps/zotonic_core/src/support/z_sitetest.erl"]
zotonic_keywords: ["how_to_guide", "backend_developer", "development_and_debugging", "validate"]
---

# Choose checks for a change

Start with the behavior that could break. A pure helper needs focused input and result checks. A resource write also needs permission and persistence checks. A template change needs a rendered page with representative content.

Compile changed Erlang code before running tests. Run the smallest relevant test first, then checks for affected integrations. Use a disposable database for tests that write data.

Record what was checked and what was not. A successful compile does not verify a browser interaction, and a successful admin request does not verify anonymous access.
