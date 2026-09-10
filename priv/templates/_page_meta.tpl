<div class="page-meta">
    <a class="page-meta__type" href="{{ id.category_id.page_url }}">
        {{ id.category_id.title }}
    </a>

    {% if id.o.in_module[1] as module_id %}
        <span aria-hidden="true">·</span>
        <a href="{{ module_id.page_url }}">{{ module_id.title }}</a>
    {% endif %}

    {% if id.is_a.releasenotes and id.name != 'doc_releasenotes_index' and id.publication_start %}
        <span aria-hidden="true">·</span>
        <time datetime="{{ id.publication_start|date:"c":"UTC"|escape }}">
            {% trans "Released {date}" date=id.publication_start|date:"F Y":"UTC" %}
        </time>
    {% elseif id.is_a.article and id.publication_start %}
        <span aria-hidden="true">·</span>
        <time datetime="{{ id.publication_start|date:"c"|escape }}">
            {% trans "Published {date}" date=id.publication_start|date:"F Y" %}
        </time>
    {% endif %}
</div>

{% with id.o.subject as subjects %}
{% with m.acl.user
        and not id.is_a.reference
        and m.acl.is_allowed.update[id]
        and m.acl.is_allowed.link[id]
        as can_edit_subjects
%}
    {% if subjects or can_edit_subjects %}
        <div class="page-subjects">
            <div id="{{ #subject_labels }}" class="page-subjects__labels">
                {% include "_page_subject_labels.tpl" id=id %}
            </div>

            {% if can_edit_subjects %}
                <button
                    id="{{ #edit_subjects }}"
                    class="subject-label-edit"
                    type="button"
                    title="{_ Edit keywords _}"
                >
                    <svg viewBox="0 0 24 24" aria-hidden="true">
                        <path d="M4 20h4l11-11-4-4L4 16v4Zm9.5-13.5 4 4" />
                    </svg>
                    <span>{_ Edit keywords _}</span>
                </button>

                {% wire
                    id=#edit_subjects
                    action={
                        dialog_open
                        intent="connect"
                        template="_action_dialog_connect.tpl"
                        title=_"Edit keywords"
                        subject_id=id
                        predicate="subject"
                        tabs_enabled=[ "find" ]
                        center=0
                        width="large"
                        action={
                            update
                            target=#subject_labels
                            template="_page_subject_labels.tpl"
                            id=id
                        }
                    }
                %}
            {% endif %}
        </div>
    {% endif %}
{% endwith %}
{% endwith %}
