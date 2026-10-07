---
name: "developer_structured_logging"
title: "Write logs that explain an operation"
summary: "Log the operation and its result with structured fields. Include an identifier that helps connect the message to a resource or job, without logging credentials or complete private request data."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_development_tools"
order: 12
required_modules: []
source_paths: ["apps/zotonic_mod_development/src/mod_development.erl", "apps/zotonic_mod_development/src/models/m_development.erl", "apps/zotonic_mod_development/priv/templates", "apps/zotonic_filehandler/src"]
zotonic_keywords: ["how_to_guide", "backend_developer", "logging_and_monitoring", "erlang_otp"]
---

# Write logs that explain an operation

Log the operation and its result with structured fields. Include an identifier that helps connect the message to a resource or job, without logging credentials or complete private request data.

```erlang
?LOG_ERROR(#{
    text => <<"Garden update failed">>,
    in => mod_garden,
    result => error,
    reason => Reason
}).
```

Use the logging macros from `zotonic_core/include/zotonic.hrl` when that header is already included. Branch on the actual result of the operation before logging success or failure.
