# Schedule deferred work

Create `src/support/garden_jobs.erl` in the active Garden application:

```erlang
-module(garden_jobs).
-include_lib("zotonic_core/include/zotonic.hrl").
-export([report/2]).

report(ResourceId, Context) ->
    case m_rsc:exists(ResourceId, Context) of
        true ->
            ?LOG_INFO(#{text => <<"Garden task completed">>,
                        in => garden_jobs, id => ResourceId}),
            ok;
        false ->
            ok
    end.
```

Compile before queueing. In a local development shell, select the site and a known practice resource:

```erlang
C = z:c(garden).
ResourceId = m_rsc:rid(home, C).
z_pivot_rsc:insert_task_after(10, garden_jobs, report,
    <<"garden-demo">>, [ResourceId], C).
```

Confirm `ResourceId` is an integer first. The queue appends the site context, so this argument list calls `report/2`, not `report/1`. Expect `{ok, TaskId}` when queued and the log message after execution. Press **Ctrl-C twice** to leave the remote shell.

Return `ok` to finish. Return `{delay, Seconds}` to retry, or `{delay, Seconds, NewArgs}` to replace the next attempt's arguments. Validate arguments again when running and handle resources deleted after enqueueing.

The key coalesces pending work for this module/function; it does not guarantee an external effect happens exactly once. For HTTP deliveries, check transport errors **and** HTTP status, use a stable idempotency key at the destination, and limit retries. Authorize the user before enqueueing; the task's context does not preserve their request-time permissions. Do not wrap every worker in `sudo` to bypass this decision.
