# Save date and time form fields

Zotonic can combine date and time form fields when converting a **list of key/value pairs** into resource properties. For example:

```erlang
Props = [{<<"dt:ymd:0:date_end">>, <<"2026-10-07">>}],
m_rsc:update(ResourceId, Props, Context).
```

The `dt:` prefix invokes date recombination; `ymd` identifies the date parts and `date_end` is the destination property. The `0` default supplies the start of the day; `1` supplies the end of the day when time parts are absent. Include the corresponding time field when the user chooses a specific time.

Use the current admin date/time templates as examples, and keep the selected timezone explicit. Test a date-only value, a date with time, an empty value and a daylight-saving boundary. Do not replace this form list with a map and expect the special field names to be recombined: direct maps should contain the final property value instead.

In a custom event handler, verify resource-edit permission, validate the submitted date and handle both `{ok, Id}` and `{error, Reason}` from the update. Reload or show success only after the update succeeds.
