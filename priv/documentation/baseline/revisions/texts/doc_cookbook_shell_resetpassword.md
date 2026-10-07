# Reset a user password from the shell

Prefer the normal password-reset flow or the admin user-management screen. For a trusted administrator recovering a **normal user account**, connect with `bin/zotonic shell` and choose the correct site:

```erlang
C = z:c(garden).
UserId = m_rsc:rid(alice_person, C).
m_rsc:p(UserId, title, C).
```

Confirm the person's identity and username in admin. Then use `m_identity:set_username_pw(UserId, Username, NewPassword, C)`, with binary values for the verified username and a new strong password. Check for `ok`; do not claim success on an error. Never use this to grant a username to an unrelated resource. Avoid recording the password in shared transcripts; the shell or terminal may retain input history.

The special `admin` login (resource 1) is controlled by the site's `admin_password` configuration. Change that at its owning configuration layer and follow the deployment's restart procedure; a normal identity update is not a substitute.

Test the login privately, then press **Ctrl-C twice** to leave the remote shell.
