---
name: "developer_testing_erlang"
title: "Test Erlang behavior with focused examples"
summary: "Keep domain logic in small functions that accept explicit inputs. Test expected results, boundary values, and meaningful error cases without starting a full site when the function does not need one."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_testing"
order: 2
required_modules: []
source_paths: ["apps/zotonic_core/test", "apps/zotonic_launcher/src/command/zotonic_cmd_runtests.erl", "apps/zotonic_core/src/support/z_sitetest.erl"]
zotonic_keywords: ["how_to_guide", "backend_developer", "development_and_debugging", "validate"]
---

# Test Erlang behavior with focused examples

Keep domain logic in small functions that accept explicit inputs. Test expected results, boundary values, and meaningful error cases without starting a full site when the function does not need one.

For code that uses Zotonic models, use the repository's existing test setup and a dedicated site context. Clean up created resources so later tests do not depend on execution order.

Select a known test module with the supported test runner. Check that the runner actually discovers it: this workspace's `runtests` command scans the core `apps/zotonic_*/test` directories, not every user application's test directory.

For the pure filter example, create `test/filter_garden_label_tests.erl` in the Garden application:

```erlang
-module(filter_garden_label_tests).
-include_lib("eunit/include/eunit.hrl").

label_test() ->
    ?assertEqual(<<"Open to visitors">>,
        filter_garden_label:garden_label(<<"open">>, undefined)),
    ?assertEqual(<<"Check visiting times">>,
        filter_garden_label:garden_label(undefined, undefined)).
```

After building the application, compile this test into a temporary directory and run it with the application's `ebin` on the path:

```sh
mkdir -p /tmp/garden-eunit
erlc -o /tmp/garden-eunit apps_user/zotonic_mod_garden/test/filter_garden_label_tests.erl
erl -noshell -pa _build/default/lib/zotonic_mod_garden/ebin /tmp/garden-eunit -eval 'case eunit:test(filter_garden_label_tests, [verbose]) of ok -> halt(0); _ -> halt(1) end.'
```

Expect one passing EUnit test. These commands run a pure helper test without a site; use the site's integration test setup for database or permission behaviour.
