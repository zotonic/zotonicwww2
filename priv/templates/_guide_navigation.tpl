{% with m.zotonicwww2_guide.navigation[id] as guide %}
{% if guide %}
<nav class="guide-navigation" aria-label="{_ Guide navigation _}">
    <ol class="guide-breadcrumbs">
        <li><a href="{{ m.rsc.page_start.page_url }}">{_ All guides _}</a></li>
        {% for part in guide.path %}
            <li>{% if part == id %}<span aria-current="page">{{ part.title }}</span>{% else %}<a href="{{ part.page_url }}?guide={{ guide.root }}">{{ part.title }}</a>{% endif %}</li>
        {% endfor %}
    </ol>
    {% if guide.parent %}
        <details class="guide-outline">
            <summary>{_ In this section _}: {{ guide.parent.title }}</summary>
            <ol>{% for item in guide.siblings %}
                <li><a href="{{ item.page_url }}?guide={{ guide.root }}"{% if item == id %} aria-current="page"{% endif %}>{{ item.title }}</a></li>
            {% endfor %}</ol>
            {% if guide.parent != guide.root %}<p><a href="{{ guide.root.page_url }}?guide={{ guide.root }}">{_ All sections in _} {{ guide.root.title }}</a></p>{% endif %}
        </details>
    {% endif %}
</nav>
{% endif %}
{% endwith %}
