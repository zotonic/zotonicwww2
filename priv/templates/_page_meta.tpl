<div class="page-meta">
    <a class="page-meta__type" href="{{ id.category_id.page_url }}">
        {{ id.category_id.title }}
    </a>

    {% if id.o.in_module[1] as module_id %}
        <span aria-hidden="true">·</span>
        <a href="{{ module_id.page_url }}">{{ module_id.title }}</a>
    {% endif %}

    {% if id.is_a.releasenotes and id.publication_start %}
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

{% if id.o.subject as subjects %}
    {% include "_subject_labels.tpl" subjects=subjects is_meta %}
{% endif %}
