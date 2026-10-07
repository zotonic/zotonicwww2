# Reviewed baseline corrections

`manifest.json` reviews all 94 captured text pages. Ninety entries replace the
English title, summary and body; four retain their bodies. `texts/` contains
cookbook replacements and two corrected HTML reference articles. Entries with
sources under the guide directories reuse that Markdown rather than duplicating
it. The original JSON exports remain unchanged.

The [page-by-page audit](../../review/baseline-2026-10-07.md) records the findings.
`installation.json` remains the mapping for the three previously reviewed
installation pages; the full manifest includes those same sources.

## Prepare the final import texts

From the workspace root:

```sh
python3 apps_user/zotonicwww2/priv/documentation/baseline/prepare.py
```

Rendering any guide with `--render` also regenerates this plan. The tool uses
`zotonicwww2_doc_link`, including its reference-code-span extension. Output is
`priv/documentation/prepared/baseline-import-plan.json`, with an HTML preview for
each replacement next to it. Generated output is ignored by Git.

The plan contains one entry per destination, final HTML properties, source URI,
snapshot hash, input hashes, required media, additive references and keywords.
It rejects incomplete reviews, changed snapshots, broken source links, unknown
reference targets and adopted tasks omitted from their final merged page.

## Apply during conversion

1. Seed missing original resources if the test needs a baseline. `seed-local.erl`
   remains a deliberately missing-only snapshot operation.
2. Import the authored guide resources, their screenshots, controlled keywords,
   and collection structure using the integration aliases. Resolve media names
   before writing any body containing `asset://` placeholders.
3. Apply **this baseline plan last**, once per resource. Match by name and source
   URI; conflicting matches are errors. Never use a production numeric ID as a
   local ID. For a production resource without a name, keep its existing identity
   and use its source URI; the local canonical name is not a request to rename it.
4. Merge English title, summary and body while preserving other languages. Ensure
   English remains in the resource's language list. Preserve name, page path,
   category, publication state and other properties. Add missing `hasreference`
   and `subject` edges without removing existing edges or touching `refers`.
5. Do not subsequently apply an individual adopted guide body or an old RST body
   over this final merged text. Review upstream importer ownership before release.

The text plan does not replace the collection/category conversion plan. Existing
fragment IDs are retained where possible; moved sections may resolve to the top
of the replacement article rather than the original paragraph.

## Local audit and application

`baseline/apply-local.erl` implements the final overlay for a seeded
`zotonicwww2` development/test site. It requires an admin context and performs no
implicit elevation. In a remote shell:

```erlang
C = z:c(zotonicwww2).
RevisionScript = filename:join([
    code:priv_dir(zotonicwww2), "documentation", "baseline", "apply-local.erl"
]).
{ok, RevisionAudit} = file:script(RevisionScript,
    [{'Context', C}, {'Mode', audit}]).
```

Check `ready`. Missing resources, media, predicates or reference/keyword targets
must be imported before applying. Input hashes must match the current sources.
A stale plan must be regenerated, not bypassed. Then:

```erlang
{ok, RevisionResult} = file:script(RevisionScript,
    [{'Context', C}, {'Mode', apply}]).
```

The script applies English replacements to existing pages too. Reruns compare
properties and only add missing connections. Each normal resource update can
still fail independently (for example due to a concurrent deletion); inspect the
result and rerun after fixing the cause. It is not a site-wide transaction or a
rollback tool. Use a test backup to repeat a conversion from its original state.

Press **Ctrl-C twice** to exit the remote shell. No import is performed by the
Python preparation step, and this local script cannot target production.
