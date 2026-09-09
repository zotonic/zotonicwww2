{% if id and m.acl.user and m.acl.is_allowed.update[id] %}
    <a
        class="page-edit-button"
        href="{% url admin_edit_rsc id=id %}"
        title="{_ Edit this page _}"
        aria-label="{_ Edit this page _}"
    >
        <svg viewBox="0 0 24 24" aria-hidden="true">
            <path d="M4 20h4l11-11-4-4L4 16v4Zm9.5-13.5 4 4" />
        </svg>
        <span>{_ Edit page _}</span>
    </a>
{% endif %}
