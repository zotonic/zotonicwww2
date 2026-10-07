# Choose a destination after signup

In the active site or module, include `zotonic.hrl` and export `observe_signup_confirm_redirect/2`:

```erlang
observe_signup_confirm_redirect(#signup_confirm_redirect{}, Context) ->
    m_rsc:p(signup_welcome, page_url, Context).
```

Create a resource named `signup_welcome`, publish it and verify the new user can view it. Compile, refresh observer registration, then complete a practice signup and its verification flow. Expect the welcome page.

This notification is for a signup with a confirmed identity. For normal login, the separate `#logon_ready_page{request_page = RequestedPage}` notification participates in destination selection. Preserve a legitimate existing destination when the flow requires it and validate any supplied URL using the authentication module's policy. Do not return an arbitrary posted URL or obsolete raw tuple.
