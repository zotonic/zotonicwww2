---
name: "developer_background_work"
title: "Run work outside a request"
summary: "Use an existing Zotonic task mechanism when it fits the job. Use a supervised worker when the work needs a persistent process with its own lifecycle."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_reusable_functionality"
order: 8
required_modules: []
source_paths: ["apps/zotonic_core/src/behaviours", "apps/zotonic_mod_base/src", "apps/zotonic_core/include/zotonic_notifications.hrl"]
zotonic_keywords: ["how_to_guide", "backend_developer", "scheduled_and_background_work", "background_task"]
---

# Run work outside a request

Use an existing Zotonic task mechanism when it fits the job. Use a supervised worker when the work needs a persistent process with its own lifecycle.

Define what happens if the process, site, or node restarts. An in-memory job is not durable simply because it runs outside the request. Make external effects retryable where possible and record enough state to distinguish a retry from a new job.

Pass the correct site context and keep authorization decisions explicit. Report failures with structured logs and a useful job identifier.

For a database-backed, deferred task, the pivot task queue accepts a module, exported function, stable job key, and argument list:

```erlang
z_pivot_rsc:insert_task_after(10, garden_jobs, refresh, JobKey, [ResourceId], C).
```

Implement `garden_jobs:refresh(ResourceId, Context)` in `src/support/garden_jobs.erl` before queuing it. The queue appends the site context. A completed callback can return `ok`; `{delay, Seconds}` requests another attempt and `{delay, Seconds, NewArgs}` changes the retry arguments. The unique key identifies pending work for that module/function; it is not a guarantee that an external side effect happens only once.

Authorize the request before queuing, validate the saved arguments again in the worker, and handle a resource deleted before execution. Verify completion in the worker's output or saved result, not just the queue-insertion response. Check the current `z_pivot_rsc` and `z_pivot_rsc_task_job` sources for failure/retry handling before relying on it for deliveries or payments.
