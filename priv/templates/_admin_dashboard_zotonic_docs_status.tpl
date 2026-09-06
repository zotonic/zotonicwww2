{% if m.acl.is_admin %}
{% with m.zotonicwww2_git.status as doc_status %}
<section class="widget" aria-labelledby="zotonic-docs-status-title">
    <div class="widget-header">{_ Zotonic documentation source _}</div>
    <div class="widget-content">
        <div class="row">
            <div class="col-md-7">
                <dl class="dl-horizontal">
                    <dt>{_ Import status _}</dt>
                    <dd>
                        {% if doc_status.status == "error" %}
                            <span class="label label-danger">{{ doc_status.status|escape }}</span>
                        {% elseif doc_status.status == "running" %}
                            <span class="label label-info">{{ doc_status.status|escape }}</span>
                        {% elseif doc_status.status == "queued" %}
                            <span class="label label-warning">{{ doc_status.status|escape }}</span>
                        {% else %}
                            <span class="label label-success">{{ doc_status.status|escape }}</span>
                        {% endif %}
                        <span class="text-muted">{{ doc_status.stage|escape }}</span>
                    </dd>
                    <dt>{_ Imported commit _}</dt>
                    <dd><code>{{ doc_status.imported_hash|default:"—"|escape }}</code></dd>
                    <dt>{_ Checkout commit _}</dt>
                    <dd><code>{{ doc_status.checkout_hash|default:"—"|escape }}</code></dd>
                    <dt>{_ Fetched commit _}</dt>
                    <dd><code>{{ doc_status.remote_hash|default:"—"|escape }}</code></dd>
                    <dt>{_ Git branch _}</dt>
                    <dd><code>{{ doc_status.branch|escape }}</code></dd>
                    <dt>{_ Started _}</dt>
                    <dd>{{ doc_status.started|default:"—"|escape }}</dd>
                    <dt>{_ Finished _}</dt>
                    <dd>{{ doc_status.finished|default:"—"|escape }}</dd>
                </dl>

                {% if doc_status.error %}
                    <div class="alert alert-danger" role="alert">
                        <strong>{_ Last error _}</strong><br>
                        <code>{{ doc_status.error|escape }}</code>
                    </div>
                {% endif %}
            </div>
            <div class="col-md-5">
                <h4>{_ Last import _}</h4>
                <table class="table table-condensed">
                    <tbody>
                        <tr><th>{_ Total _}</th><td>{{ doc_status.total|escape }}</td></tr>
                        <tr><th>{_ Created _}</th><td>{{ doc_status.created|escape }}</td></tr>
                        <tr><th>{_ Updated _}</th><td>{{ doc_status.updated|escape }}</td></tr>
                        <tr><th>{_ Unchanged _}</th><td>{{ doc_status.unchanged|escape }}</td></tr>
                        <tr><th>{_ Deprecated _}</th><td>{{ doc_status.deprecated|escape }}</td></tr>
                    </tbody>
                </table>

                {% if doc_status.keyword_coverage %}
                    <h4>{_ Keyword coverage _}</h4>
                    <p class="text-muted">
                        {_ An imported page is covered when it has at least one subject keyword. _}
                    </p>
                    <div class="table-responsive">
                        <table class="table table-condensed">
                            <thead>
                                <tr>
                                    <th>{_ Category _}</th>
                                    <th>{_ Covered _}</th>
                                    <th>{_ Missing _}</th>
                                    <th>{_ Coverage _}</th>
                                    <th>{_ Keywords _}</th>
                                </tr>
                            </thead>
                            <tbody>
                                {% for coverage in doc_status.keyword_coverage %}
                                    <tr>
                                        <td>{{ m.rsc[coverage.category_id].title|default:coverage.category|escape }}</td>
                                        <td>{{ coverage.covered|escape }} / {{ coverage.total|escape }}</td>
                                        <td>{{ coverage.missing|escape }}</td>
                                        <td>{{ coverage.coverage_percent|escape }}%</td>
                                        <td>{{ coverage.keyword_count|escape }}</td>
                                    </tr>
                                {% endfor %}
                            </tbody>
                        </table>
                    </div>
                {% endif %}

                <h4>{_ Migration _}</h4>
                <p class="text-muted">
                    {% trans "{tracked} source pages tracked; {legacy} legacy candidates remain."
                        tracked=doc_status.migration.tracked
                        legacy=doc_status.migration.legacy_candidates
                    %}
                </p>
            </div>
        </div>

        <hr>

        <div class="btn-toolbar" role="toolbar" aria-label="{_ Documentation import actions _}">
            {% button
                class="btn btn-primary"
                text=_"Fetch and rebuild"
                title=_"Fetch the latest master commit, compile it, import all documentation, and publish EDoc."
                postback=`docs_update`
                delegate=`zotonicwww2`
            %}
            {% button
                class="btn btn-default"
                text=_"Fetch only"
                title=_"Fetch and select the latest master commit without compiling or importing it."
                postback=`docs_fetch`
                delegate=`zotonicwww2`
            %}
            {% button
                class="btn btn-default"
                text=_"Rebuild checkout"
                title=_"Compile and import the currently selected checkout without fetching."
                postback=`docs_rebuild`
                delegate=`zotonicwww2`
            %}
            {% button
                class="btn btn-default"
                text=_"Import compiled docs"
                title=_"Import documentation from the existing compiled checkout."
                postback=`docs_import`
                delegate=`zotonicwww2`
            %}
            {% button
                class="btn btn-default"
                text=_"Import all keywords"
                title=_"Create or update all subject keywords in their corresponding keyword sub-category."
                postback=`docs_import_keywords`
                delegate=`zotonicwww2`
            %}
            {% if doc_status.migration.legacy_candidates %}
                {% button
                    class="btn btn-warning"
                    text=_"Migrate legacy imports"
                    title=_"Adopt and deprecate source-style pages left over from the old importer."
                    action={confirm
                        text=_"This will unpublish remaining legacy source-style pages which are absent from the latest successful import. Continue?"
                        postback=`docs_migrate_legacy`
                        delegate=`zotonicwww2`
                        ok=_"Migrate"
                    }
                %}
            {% endif %}
        </div>

        <p class="help-block">
            {_ GitHub push deliveries should be sent to _}
            <code>{% url github_webhook %}</code>.
            {_ The shared secret is configured as site.rebuild_secret. _}
        </p>
    </div>
</section>
{% endwith %}
{% endif %}
