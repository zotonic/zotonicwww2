---
name: "admin_test_email"
title: "Direct test mail to a controlled recipient"
summary: "Prevent an acceptance workflow from emailing real users."
category: "adminguide"
language: "en"
is_published: true
parent: "admin_collection_services"
order: 4
required_modules: []
zotonic_keywords: ["how_to_guide", "operator", "email_delivery", "configuration", "privacy"]
---

# Direct test mail to a controlled recipient

**Access needed:** Site email-settings permission or server configuration access.

Set up test-mail routing before running copied production data or testing scheduled mail.

1. Choose a mailbox that the testing team controls.
2. Configure the appropriate `email_override`: the global setting affects the installation; the site's `site.email_override` affects that site. Check any configured exceptions.
3. Apply the change using the owning configuration mechanism.
4. Send a test whose requested recipient is another address you control. Verify the actual delivery destination and inspect the mail status.
5. Test the real workflow, then record that the override is intentional for this environment.
6. Before production cutover, check the effective override and exceptions explicitly. A leftover catch-all can divert real users' messages.

This setting governs Zotonic's email handling. Custom integrations that send through another provider API need their own test configuration. Do not assume a database copy also copied safe delivery settings.
