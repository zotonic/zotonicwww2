# Create an Erlang application with Rebar3

This exercise runs outside the Zotonic workspace. Use Erlang/OTP **28** and [Rebar3](https://rebar3.org/docs/getting-started/). Check `erl` and `rebar3 version` before starting.

## Create and compile

In a directory for practice projects:

```sh
rebar3 new app garden_counter
cd garden_counter
rebar3 compile
rebar3 shell
```

The generated application contains `src/garden_counter.app.src`, `src/garden_counter_app.erl`, and `src/garden_counter_sup.erl`. Erlang source belongs in `src`; generated beam files are under `_build/default/lib/garden_counter/ebin`.

At the Erlang prompt:

```erlang
application:ensure_all_started(garden_counter).
application:which_applications().
```

Starting an already running application is harmless. Check that `garden_counter` appears in the application list. The application callback starts the supervisor; the generated supervisor initially has no workers.

`rebar3 shell` starts a separate practice VM. You can use `q().` to stop **that VM**. In a remote Zotonic shell, press **Ctrl-C twice** to leave; do not call `q().` or `halt().` there.

## Add a test

Create `test/garden_counter_tests.erl`:

```erlang
-module(garden_counter_tests).
-include_lib("eunit/include/eunit.hrl").

application_metadata_test() ->
    case application:load(garden_counter) of
        ok -> ok;
        {error, {already_loaded, garden_counter}} -> ok
    end,
    {ok, Description} = application:get_key(garden_counter, description),
    ?assert(is_list(Description)).
```

Run `rebar3 eunit`. Expect the test to pass. Continue with the supervised-worker recipe to add useful behaviour. Zotonic applications use Zotonic's own workspace build; do not run `rebar3 new` inside an existing site directory.
