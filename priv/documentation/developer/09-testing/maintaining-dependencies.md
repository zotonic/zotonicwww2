---
name: "developer_maintaining_dependencies"
title: "Update dependencies deliberately"
summary: "Change a dependency when you can identify the required fix or feature and the affected callers. Read its version constraints and inspect the resulting lockfile change."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_testing"
order: 6
required_modules: []
source_paths: ["apps/zotonic_core/test", "apps/zotonic_launcher/src/command/zotonic_cmd_runtests.erl", "apps/zotonic_core/src/support/z_sitetest.erl"]
zotonic_keywords: ["how_to_guide", "backend_developer", "module_management", "maintainability"]
---

# Update dependencies deliberately

Change a dependency when you can identify the required fix or feature and the affected callers. Read its version constraints and inspect the resulting lockfile change.

Build from a clean dependency state when verifying a release. Test the integration behavior rather than relying solely on compilation. For frontend dependencies, check the generated assets and the browser as well.

Keep unrelated dependency updates out of a focused application change. Document any configuration or runtime requirement that changes with the update.
