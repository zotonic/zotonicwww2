{% with id.depiction|default:id as preview_id %}
    <article class="home-media-card home-media-card--{{ kind }} do_clickable">
        <figure class="home-media-card__visual" aria-hidden="true">
            {% image preview_id mediaclass="home-media" alt="" %}
            <span class="home-media-card__type">
                {% if kind == "video" %}
                    <svg viewBox="0 0 24 24"><path d="m9 7 8 5-8 5z" /></svg>
                {% else %}
                    PDF
                {% endif %}
            </span>
        </figure>
        <div class="home-media-card__copy">
            <p class="home-media-card__meta">
                {% if kind == "video" %}
                    {_ Video _}
                {% else %}
                    {_ PDF document _}
                    {% if m.media[id].size %}<span>{{ m.media[id].size|filesizeformat }}</span>{% endif %}
                {% endif %}
            </p>
            <h4><a href="{{ id.page_url }}">{{ id.title }}</a></h4>
            {% if id|summary:110 as item_summary %}
                <p class="home-media-card__summary">{{ item_summary }}</p>
            {% endif %}
        </div>
    </article>
{% endwith %}
