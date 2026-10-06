{% if m.acl.is_admin %}
    <h3>{{ repo.title|escape }}</h3>
    <p>{_ Status _}: <strong>{{ repo.status|escape }}</strong></p>
    {% if repo.error %}<pre>{{ repo.error|escape }}</pre>{% endif %}
    <h4>{_ Imported pages _} ({{ repo.imported_count }})</h4>
    <ul>
        {% for row in repo.report %}
            {% if row.rsc_id %}<li><a href="{{ m.rsc[row.rsc_id].page_url }}">{{ row.module|escape }}</a> — {{ row.path|escape }}</li>{% endif %}
        {% endfor %}
    </ul>
    <h4>{_ Pages not imported _} ({{ repo.skipped_count }})</h4>
    <ul>
        {% for row in repo.report %}
            {% if not row.rsc_id %}<li><code>{{ row.path|escape }}</code>: {{ row.error|escape }}</li>{% endif %}
        {% endfor %}
    </ul>
    <div class="modal-footer">
        {% button text=_"Close" action={dialog_close} class="btn btn-default" %}
    </div>
{% endif %}
