---
name: "developer_model_errors"
title: "A model returns an error or no value"
summary: "Check the requested path and compare its binary segments with the model callback patterns. Confirm that the callback returns the unused path in the required result structure."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_troubleshooting"
order: 6
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command", "apps/zotonic_mod_development", "apps/zotonic_filehandler/src"]
zotonic_keywords: ["troubleshooting", "backend_developer", "model", "development_and_debugging"]
---

# A model returns an error or no value

Check the requested path and compare its binary segments with the model callback patterns. Confirm that the callback returns the unused path in the required result structure.

Next, check permissions using the same user and context as the failing request. A successful local admin shell call does not prove that a visitor should receive the value.

Log the operation's error reason without dumping private payloads. Handle unknown paths and expected access errors separately from unexpected failures so callers can respond appropriately.
