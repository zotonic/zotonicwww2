---
name: "developer_testing_permissions"
title: "Test with different users"
summary: "Choose roles that exercise the operation's permission boundary: anonymous visitor, limited editor, and administrator. Use separate browser sessions or an explicit test context for each role."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_testing"
order: 4
required_modules: []
source_paths: ["apps/zotonic_core/test", "apps/zotonic_launcher/src/command/zotonic_cmd_runtests.erl", "apps/zotonic_core/src/support/z_sitetest.erl"]
zotonic_keywords: ["how_to_guide", "backend_developer", "authorization_and_access_control", "validate", "security"]
---

# Test with different users

Choose roles that exercise the operation's permission boundary: anonymous visitor, limited editor, and administrator. Use separate browser sessions or an explicit test context for each role.

Test the direct model or endpoint as well as the visible interface. Confirm that denied writes leave the data unchanged and do not reveal private fields in the error response.

Include unpublished resources and resources outside the user's content group where relevant. Record the role and resource used in the test so someone else can reproduce the result.
