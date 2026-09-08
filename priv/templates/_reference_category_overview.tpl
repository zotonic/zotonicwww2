{% with [
    'module',
    'controller',
    'model',
    'dispatch',
    'template_tag',
    'template_filter',
    'template_action',
    'template_scomp',
    'template_validator',
    'template',
    'notification'
] as reference_category_names %}
<nav class="reference-category-overview" aria-labelledby="reference-category-overview-title">
    <header>
        <p class="reference-category-overview__eyebrow">{_ Reference sections _}</p>
        <h2 id="reference-category-overview-title">{_ Browse by component _}</h2>
        <p>{_ Start with the part of Zotonic you are working with, or use the searchable index below to narrow the complete reference by category, topic, or module. _}</p>
    </header>
    <ul class="reference-category-overview__list">
        {% for category_name in reference_category_names %}
            {% if m.rsc[category_name].id as category_id %}
                <li>
                    <a href="{{ category_id.page_url }}">
                        <h3>{{ category_id.title }}</h3>
                        {% if category_id.summary %}
                            <p>{{ category_id.summary }}</p>
                        {% endif %}
                    </a>
                </li>
            {% endif %}
        {% endfor %}
    </ul>
</nav>
{% endwith %}
