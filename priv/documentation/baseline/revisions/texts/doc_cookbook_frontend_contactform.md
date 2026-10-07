# Add a contact form

For an ordinary contact form, enable `module#mod_survey`, create a form, add the required questions, and configure its response email. Test required fields, an invalid email address, and a successful submission. Use the editor survey tasks for the complete workflow.

## When a custom handler is needed

Build a wired form and an exported `event/2` handler in an active module. Use validators for required fields and email syntax. In a `#submit{}` handler, read fields with `z_context:get_q_validated(<<"email">>, Context)` and equivalent binary keys. Do not substitute `get_q` and assume the browser's checks are enough.

Send to a **configured recipient**, not a posted destination. Pass the visitor's address as a reply-to value through the mail API, not as raw headers. Render the email with a template that escapes submitted text. For a basic template email, use `z_email:send_render/4`. When a reply-to address is needed, use `z_email:send/2` with an `#email{reply_to = Address, html_tpl = Template, vars = Vars, to = Recipient}` record. Handle `{ok, MessageId}` and `{error, Reason}` separately before updating the form. Queued mail is not proof of delivery.

Keep the fields when sending fails and show a useful retry message. Add rate limiting or the installation's spam controls before exposing the form publicly. Do not log the message body or dump all submitted values. Test the mail queue, recipient delivery and failure path in acceptance.

The old `mod_emailer` call and unvalidated example are obsolete. Refer to `module#mod_survey` and the current `z_email` source in `zotonic_core` when implementing a custom delivery path.
