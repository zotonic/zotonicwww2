---
name: "admin_assign_access"
title: "Assign user groups and check access"
summary: "Give an account the permissions required for its work."
category: "adminguide"
language: "en"
is_published: true
parent: "admin_collection_people"
order: 2
required_modules: ["mod_acl_user_groups"]
zotonic_keywords: ["how_to_guide", "site_administrator", "identity_and_accounts", "authorization_and_access_control"]
---

# Assign user groups and check access

**Access needed:** Site administrator with user-management and access-rule permissions.

Start with an existing account and a clear description of its work: which content it maintains, whether it publishes, and whether it manages other people.

1. Find the person's page in the user overview and open it.
2. In **User groups**, select the existing group that matches the agreed role. Check inherited memberships and save.
3. Use a separate browser profile to sign in as a representative test account with those memberships.
4. Check one allowed operation, such as editing a draft in the intended content group.
5. Check one restriction, such as editing another team's content or opening user management.
6. Record the membership change and the person responsible for approving it.

Do not give an account administrator access merely to fix one missing permission. If no existing role fits, have an access-rule administrator adjust the rules and test them before publication. More than one group can affect the effective permissions.

A person page, login identity, user group, and content group have different purposes. A collection used for navigation is not an access-control boundary.
