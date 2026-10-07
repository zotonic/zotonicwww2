# Add a supervised Erlang worker

Start with the `garden_counter` application from the preceding Rebar3 recipe. Use OTP 28. Create `src/garden_counter_server.erl`:

```erlang
-module(garden_counter_server).
-behaviour(gen_server).
-export([start_link/0, next/0]).
-export([init/1, handle_call/3, handle_cast/2]).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
next() -> gen_server:call(?MODULE, next).
init([]) -> {ok, 0}.
handle_call(next, _From, Count) ->
    {reply, Count + 1, Count + 1};
handle_call(_Request, _From, Count) ->
    {reply, {error, unknown_request}, Count}.
handle_cast(_Message, Count) -> {noreply, Count}.
```

Replace the generated supervisor's `init/1` definition with:

```erlang
init([]) ->
    Flags = #{strategy => one_for_one, intensity => 3, period => 10},
    Worker = #{id => garden_counter_server,
               start => {garden_counter_server, start_link, []},
               restart => permanent,
               shutdown => 5000,
               type => worker,
               modules => [garden_counter_server]},
    {ok, {Flags, [Worker]}}.
```

Keep its existing `-module`, behaviour, exports and `start_link/0`. Then run `rebar3 compile` and `rebar3 shell` again. In that practice shell:

```erlang
application:ensure_all_started(garden_counter).
garden_counter_server:next().
garden_counter_server:next().
supervisor:which_children(garden_counter_sup).
```

Expect 1, then 2, and a running worker PID. To test recovery **in this practice application**, stop the worker:

```erlang
gen_server:stop(garden_counter_server).
```

Call `supervisor:which_children/1` again after it restarts. The PID should change. The next counter value is 1: supervision restarts a process, but does not persist its state. Rapid repeated failures can exceed the restart limit and stop the supervisor.

For graphical inspection, `observer:start().` requires Erlang's optional wx support and a display. `supervisor:which_children/1` works without a GUI; the old `pman` tool is not part of this workflow. Leave the standalone practice shell with `q().`. Leave a remote Zotonic shell with **Ctrl-C twice**.
