# Use an Erlang shell safely

Use Erlang/OTP 28 for development. Start a standalone practice VM with `erl`. Expressions end with a period:

```erlang
2 + 3.
Name = <<"Garden">>.
#{name => Name, count => 5}.
f(Name).
erlang:system_time(second).
```

Variables can be bound once; `f(Name).` forgets a shell binding. `help().` lists shell commands. Use `erlang:monotonic_time/0` and `erlang:convert_time_unit/3` for elapsed durations; system time can change when the clock is corrected.

## Connect to Zotonic

From the installation directory, use `bin/zotonic shell`. Select a site with `C = z:c(garden).` before calling its models. This shell has administrative capabilities. Do not paste commands from an untrusted source.

Press **Ctrl-C twice** to exit the remote shell. Do not use `q().`, `halt().`, or `init:stop().` to disconnect: they stop the connected runtime. `q().` is appropriate only when you intend to stop a standalone practice VM.
