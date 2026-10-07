# Assign a user group from a trusted creation workflow

A person resource and a login identity are separate. If your administrative workflow creates a person in a special category, add the membership **after** resource creation succeeds. Do not look up the category of an undefined ID inside an insert observer.

For example, create a `vip` subcategory of `person` and a group named `acl_user_group_vips`. Review that group's permissions before using it. Add this helper to your site application as `src/support/garden_membership.erl`:

```erlang
-module(garden_membership).
-export([assign/2]).

assign(UserId, Context) ->
    case z_acl:is_admin(Context) andalso m_rsc:is_a(UserId, vip, Context) of
        true ->
            case m_rsc:rid(acl_user_group_vips, Context) of
                undefined -> {error, missing_group};
                GroupId -> m_edge:insert(UserId, hasusergroup, GroupId, Context)
            end;
        false ->
            {error, eacces}
    end.
```

Compile, then call `garden_membership:assign(UserId, Context)` from the trusted creation workflow after `m_rsc:insert/2` has returned `{ok, UserId}`. Expect `{ok, EdgeId}`; handle an error instead of reporting that the complete workflow succeeded. Keep the authenticated administrator context so both this check and the edge model's checks apply. Do not add `sudo` to make a public signup pass.

Test a VIP, another person category, a non-administrator and a missing group. Confirm the `hasusergroup` connection in admin and test the account's resulting permissions. Repeat assignment to confirm it does not create duplicate membership.

Ordinary authenticated users without explicit group edges fall back to `acl_user_group_members`. Once explicit groups are assigned, that fallback no longer supplies membership; configure group inheritance deliberately if VIPs should retain those permissions. A broad resource-update observer is a poor place to grant elevated membership based on a category that a visitor might influence.
