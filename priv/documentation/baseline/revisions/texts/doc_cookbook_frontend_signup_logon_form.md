# Customize login and signup presentation

Start with `module#mod_authentication`; enable `module#mod_signup` only if public registration is intended. Confirm the unmodified login, logout, password-reset and signup flows work before changing templates.

1. Inspect `logon.tpl` in `zotonic_mod_authentication` and `signup.tpl` in `zotonic_mod_signup` for the current include chain.
2. Override the smallest presentation template in the site's `priv/templates`, using the same filename. Preserve the supplied form actions, field names, validation, scripts and configuration includes.
3. Match the site's layout and labels. Avoid copying the entire authentication implementation into a custom form.
4. Test valid and invalid passwords, reset links, verification-required signup, rate limits, and any enabled external identity provider.

Do not replace the configured password policy with a hard-coded length, or silently submit a hidden “terms accepted” value. If the site requires acceptance, present the terms and record the visitor's explicit choice using the signup flow.

The old standalone form markup predates the current authentication implementation. Use the installed templates as the source of truth when upgrading.
