# Enable a module from the Erlang shell

Compile the module and confirm Zotonic can discover its OTP application. From the installation directory, run `bin/zotonic shell` as the service account, using the same node and cookie configuration as the running server.

```erlang
C = z:c(garden).
z_module_manager:activate(mod_garden, C).
z_module_manager:active(mod_garden, C).
```

Replace `garden` and `mod_garden` with your site and module. Inspect the activation result and logs; do not assume a queued activation has completed. The final call should return `true`. A module can still have an application-specific configuration error after activation.

If the shell cannot connect, check the actual node name, short-name versus long-name mode, hostname resolution and cookie permissions. A fully qualified hostname is not universally required. Press **Ctrl-C twice** to exit the remote shell.
