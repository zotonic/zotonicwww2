---
name: "admin_outbound_email"
title: "Configure outbound email and verify delivery"
summary: "Set a relay at the correct scope and test a real site message."
category: "adminguide"
language: "en"
is_published: true
parent: "admin_collection_services"
order: 3
required_modules: []
zotonic_keywords: ["how_to_guide", "operator", "email_delivery", "configuration"]
---

# Configure outbound email and verify delivery

**Access needed:** Site email-settings permission; server access for global relay settings.

Get the provider's relay hostname, port, authentication method, TLS requirements, and permitted sender address before changing settings.

1. Decide whether the relay is global or specific to this site. Inspect existing overrides first.
2. Configure the provider's host, port, credentials, and TLS mode at that scope. Global keys include `smtp_relay`, `smtp_host`, `smtp_port`, `smtp_username`, `smtp_password`, and `smtp_ssl`; site relay settings use different `smtp_relay_*` keys.
3. Apply the documented reload or restart and check the effective settings without exposing credentials.
4. Arrange the provider's required domain verification and DNS records with the domain owner.
5. Send one representative message to an address you control, such as a password-recovery message or form receipt.
6. Check Zotonic's mail status, the provider's delivery status, and the recipient inbox. Confirm the sender and reply address are correct.

Server acceptance and inbox delivery are different outcomes. Follow the mail-diagnosis task if delivery fails. Never disable certificate checks to work around a TLS mismatch; verify the provider's hostname and connection mode.
