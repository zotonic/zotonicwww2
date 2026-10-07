---
name: "developer_cmd_etop"
title: "etop: Inspect active Erlang processes"
summary: "Inspect active Erlang processes."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_reference"
order: 15
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command/zotonic_cmd_etop.erl"]
zotonic_keywords: ["reference", "backend_developer", "logging_and_monitoring", "performance"]
---

# etop: Inspect active Erlang processes

Inspect active Erlang processes.

## Syntax

```text
bin/zotonic etop 
```

## Requirements and effects

Requires a reachable node and the optional OTP etop tool.

Displays a text view of up to 25 processes with etop tracing disabled by this wrapper. Use it to narrow a performance investigation, then correlate activity with a specific request or job. A busy process is evidence to investigate, not automatically a defect.

## Example

```sh
bin/zotonic etop
```

Run commands from the Zotonic workspace root. Replace `garden` with your site name where applicable.
