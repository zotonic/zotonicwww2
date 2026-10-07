---
name: "admin_remove_access"
title: "Remove access when someone leaves"
summary: "Remove login methods and permissions without deleting their authored content."
category: "adminguide"
language: "en"
is_published: true
parent: "admin_collection_people"
order: 4
required_modules: []
zotonic_keywords: ["how_to_guide", "site_administrator", "identity_and_accounts", "authentication", "authorization_and_access_control"]
---

# Remove access when someone leaves

**Access needed:** Site administrator; identity-provider or integration access may also be needed.

First identify the person, the accounts they use, and which content or responsibilities need transferring.

1. Review their user groups, collaboration memberships, API credentials, and any external sign-in method.
2. Remove the memberships that grant access and save.
3. For a local username/password account, open **Set username / password** and choose **Delete Username**. Read the confirmation carefully.
4. Disable the account at an external identity provider and revoke separate integration tokens where applicable.
5. Arrange invalidation of existing sessions with the site's authentication administrator; removing a password alone is not proof that every existing session is gone.
6. Test that the former access methods fail and that the replacement owner can still do the work.

Keep the person resource when it is needed for authorship or records. Unpublishing a person page is not an account-deactivation procedure. Do not delete their content merely to remove a login.

The built-in `admin` account is configured separately; this username-deletion dialog does not manage it. Handle emergency administrator credential rotation through site configuration.
