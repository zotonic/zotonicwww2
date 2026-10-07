---
name: "developer_browser_messaging"
title: "Use Cotonic and MQTT for browser messaging"
summary: "Use the existing Cotonic browser models and Zotonic MQTT bridge when components need to communicate through topics. First identify whether the topic stays inside the browser or crosses to the server."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_browser_apis"
order: 1
required_modules: []
source_paths: ["apps/zotonic_mod_wires/src/actions", "apps/zotonic_mod_base/priv/lib/js", "apps/zotonic_mod_mqtt", "apps/zotonic_mod_oauth2"]
zotonic_keywords: ["how_to_guide", "frontend_developer", "messaging_and_pubsub", "cotonic", "mqtt"]
---

# Use Cotonic and MQTT for browser messaging

Use the existing Cotonic browser models and Zotonic MQTT bridge when components need to communicate through topics. First identify whether the topic stays inside the browser or crosses to the server.

Define a small payload and a clear reply contract. Subscribe before requesting data when the response can arrive immediately. Clean up component subscriptions when their lifetime ends, and handle reconnects without duplicating actions.

Server-side topic access must follow the site's permission rules. A topic name is not a secret or an access check.

With the greeting model from the model task active, try this read in the browser console:

```javascript
cotonic.ready.then(() =>
    cotonic.broker.call("bridge/origin/model/garden/get/greeting", {})
).then(reply => console.log(reply.payload));
```

Inspect the reply's status and result; expect the greeting on success. An error or disconnected bridge is not an empty greeting. The `bridge/origin/` prefix crosses to the site's server model, while a browser-local model such as `model/location` stays in the browser. Use `cotonic#location` for the latter's topic contract. Do not insert response text as HTML without escaping it.
