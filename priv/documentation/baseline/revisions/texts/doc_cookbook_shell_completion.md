# Complete module and function names in the shell

Connect with `bin/zotonic shell`. Type part of a module name and press Tab, then try a module followed by a colon and part of a function name. Completion depends on the running OTP shell, the code path, and module availability; it does not require restarting the site in debug mode.

If your new module is missing, compile it through the workspace workflow, confirm its application is discovered, and inspect:

```erlang
code:which(mod_garden).
code:ensure_loaded(mod_garden).
mod_garden:module_info(exports).
```

A filename and `{module, mod_garden}` confirm availability. `non_existing` or `{error, nofile}` means the module is not on this node's code path. Fix that before investigating completion. Press **Ctrl-C twice** to leave the remote shell.
