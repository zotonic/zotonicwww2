# Run site-specific work after signup

Add the following to an active module, merging the include and export declarations:

```erlang
-include_lib("zotonic_core/include/zotonic.hrl").
-export([observe_signup_done/2]).

observe_signup_done(#signup_done{id = Id, is_verified = IsVerified}, _Context) ->
    ?LOG_INFO(#{text => <<"Garden signup completed">>,
                in => mod_garden, id => Id, is_verified => IsVerified}),
    ok.
```

Compile, refresh observer registration and create a practice account. Confirm a log entry with the new resource ID. The current record also has `props` (a map) and `signup_props` (a list); use the header instead of a positional tuple.

`signup_done` does not mean every identity is verified. Gate sensitive provisioning on the required verification event or state, and check it again when deferred work executes. For slow provisioning, queue a task keyed by the user and operation. Check whether the destination account already exists before creating it; retries and repeated notifications must not create duplicate accounts.

Use `notification#signup_check` for checks before creation and `notification#signup_confirm` for confirmation follow-up. Do not grant editor or administrator groups automatically to public signups.
