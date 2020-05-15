<div class="list-item{% if is_highlight or id.is_featured %} featured{% endif %}">
    <h3>
        <a href="{{ id.page_url }}">
            {{ id.title }}
            <span class="text-muted">{{ id.category_id.title|lower }}</span>
        </a>
    </h3>
    <p>
        {{ id|summary }}
    </p>
</div>
