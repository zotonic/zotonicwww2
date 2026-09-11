{% if id %}
    <article class="category-index__card">
        <a href="{{ id.page_url }}">
            <h3>{{ id.title }}</h3>
            <p>{{ id.summary|default:description }}</p>
            <span>{_ Open guide → _}</span>
        </a>
    </article>
{% endif %}
