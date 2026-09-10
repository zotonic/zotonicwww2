{% if id %}
    <article class="home-release-list__item do_clickable">
        <p class="home-entry__type">
            <span>{_ Release notes _}</span>
            {% if id.publication_start %}
                <time datetime="{{ id.publication_start|date:"c":"UTC" }}">
                    {{ id.publication_start|date:_"j M Y":"UTC" }}
                </time>
            {% endif %}
        </p>
        <h3><a href="{{ id.page_url }}">{{ id.title }}</a></h3>
        <p class="home-entry__summary">{{ id|summary:180 }}</p>
    </article>
{% endif %}
