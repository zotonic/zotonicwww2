{% for id in search.result_ids %}
    {% if id.is_visible %}
        <article class="search-result-card">
            <a href="{{ id.page_url }}">
                <p class="search-result-card__meta">
                    {{ id.category_id.title }}
                </p>
                <h3>{{ id.title|default:_"Untitled" }}</h3>
                {% if id.summary %}
                    <p class="search-result-card__summary">{{ id|summary:180 }}</p>
                {% endif %}
            </a>
        </article>
    {% endif %}
{% endfor %}

{% if search.pager.next and not hide_loader %}
    <div id="search-more-{{ search.pager.next }}"
         class="search-result-list__loader"
         role="status"
         data-onvisible-topic="model/loadmore/post/replace"
         data-template="_search_results_page.tpl"
         data-url="{% url none
            page=search.pager.next
            qs=search.query
            category=search.category
            subject=search.subject
            module=search.module
         %}">
        {_ Loading more results… _}
    </div>
{% endif %}
