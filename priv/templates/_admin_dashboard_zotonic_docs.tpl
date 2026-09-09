{% if m.acl.is_admin %}
    <div id="zotonic-docs-dashboard">
        {% include "_admin_dashboard_zotonic_docs_status.tpl" %}
    </div>

    {% live
        template="_admin_dashboard_zotonic_docs_status.tpl"
        target="zotonic-docs-dashboard"
        method="updateonly"
        topic="bridge/origin/model/zotonicwww2_git/event/status"
    %}
{% endif %}
