# Import all documentation

`zotonicwww2_documentation_import` imports the editor, developer and administration
guides, reviewed baseline articles and cookbooks, screenshots, keywords and ordered
connections. Run it in an Erlang shell on the destination server after deploying
zotonicwww2. It uses normal Zotonic model APIs; no OAuth key is needed for this
server-side import.

## Prepare and deploy

From the Zotonic workspace root, with the application compiled:

```sh
python3 apps_user/zotonicwww2/priv/documentation/build_import_plan.py
```

This validates and renders all sources with the site's Markdown extension and
writes `priv/documentation/import/plan.json`. Commit and deploy that file together
with the documentation sources, images, keyword taxonomy and application code.
Rebuild the plan after changing its inputs. Python is only needed when building
the plan, not on the destination server. The Erlang importer verifies input hashes
and rejects an out-of-date plan.

Compile and deploy the complete application, including schema 27 and the updated
`zotonicwww2_doc_import` ownership checks. Start the site and allow its schema
upgrade and reference documentation import to finish first. Required categories,
predicates and referenced documentation pages must exist; audit reports missing
resources rather than creating placeholder reference pages.

## Audit the local site

From the Zotonic installation directory:

```sh
bin/zotonic shell
```

Create an administrator context explicitly and name the expected destination:

```erlang
C = z_acl:sudo(z:c(zotonicwww2)).
Options = #{site => zotonicwww2, environment => development}.
{ok, Audit} = zotonicwww2_documentation_import:audit(Options, C),
io:format("~p~n", [maps:with([site, environment, pages, media, errors], Audit)]).
```

Audit does not write to the site. `create` and `existing` in its result list the
resource names that will be created or reused. Resolve every entry in `errors`
before importing. A destination mismatch, non-admin context or invalid bundle
returns `{error, Reason}`. Audit cannot guarantee remote image downloads will
succeed; seven baseline images are reused where present or downloaded from
zotonic.com when missing.

## Apply and review

```erlang
Result = zotonicwww2_documentation_import:import(Options, C),
case Result of
    {ok, Report} -> io:format("Imported into ~p: ~p pages, ~p media.~n",
        [maps:get(site, Report), length(maps:get(resources, Report)), length(maps:get(media, Report))]);
    {error, Reason} -> io:format("Import failed: ~p~n", [Reason])
end.
```

The import repeats the audit before writing. It imports the controlled keyword
taxonomy, media, pages, final corrected English texts, and connections in that
order. You do not need to run `baseline/seed-local.erl` or `baseline/apply-local.erl`
first. Those remain separate fixture tools.

New content is published immediately. Existing pages retain their publication
state, category, name, page path and other languages. Names and recorded source
URIs identify existing pages; production numeric IDs are never reused as local IDs.
The final baseline revisions take precedence over guide aliases so merged pages
receive the reviewed text. Later source-documentation refreshes skip pages marked
`doc_editorial_bundle`.

Imported connections use `haspart`, `relation`, `hasreference`, `subject` and
`depiction`. The `doc_editorial_edges` ledger records edges actually added by this
importer. Subsequent runs remove obsolete owned edges and retain other connections.
Source order comes first; additional existing edges keep their relative order
after it. Resources removed from the bundle are retained for manual review.

Review `/start`, each guide root and several illustrated tasks while logged out.
Check existing translated pages as well. Repeating the import with the same plan
is supported: existing resources are reused, unchanged texts and connections are
skipped, and unchanged screenshots are not uploaded again.

The import is **not one database transaction**: media files and completed stages
remain if a later stage fails. Fix the reported problem and run it again. Take the
normal database and media backup before the production run. No automatic rollback
of a completed import is provided.

Leave the remote shell by pressing **Ctrl-C twice**.

## Production

Deploy exactly the reviewed bundle and importer to the production installation.
Run the same commands in that server's Erlang shell. Use the actual production
site atom in `z:c(...)` and in `Options`, and set `environment => production`.
Both values must match the running site. Audit there before applying: a clean
local audit does not prove all production reference pages are available.

## Verification

The plan covers 383 documentation resources and 28 media resources (21 guide
screenshots and seven baseline images). The local audit and rollback-only Erlang
integration checks are recorded in the task report; running them does not import
the full bundle.

Pure Erlang tests are available when compiling the importer with `TEST` defined.
`integration_test/1`, also only exported in that build, checks page retries,
publication preservation, translations, ordered and removed owned connections,
link resolution and protection from the older source importer. It requires an
administrator context on a development site and rolls back its fixture rows.
