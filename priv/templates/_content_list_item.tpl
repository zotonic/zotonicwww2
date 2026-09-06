{% if id.is_visible %}
    {% with depiction|default:id.depiction as item_depiction %}
        <article class="content-list__item{% if item_depiction %} content-list__item--media{% endif %}{% if is_highlight or id.is_featured %} content-list__item--highlight{% endif %} do_clickable">
            <p class="content-list__meta">
                <span>{{ label|default:id.category_id.title }}</span>
                {% if id.is_a.releasenotes and id.org_pubdate %}
                    <time datetime="{{ id.org_pubdate|date:"c":"UTC" }}">
                        {{ id.org_pubdate|date:_"j M Y":"UTC" }}
                    </time>
                {% elseif id.is_a.article and id.publication_start %}
                    <time datetime="{{ id.publication_start|date:"c" }}">
                        {{ id.publication_start|date:_"j M Y" }}
                    </time>
                {% endif %}
            </p>

            <div class="content-list__copy">
                <h3 class="content-list__title">
                    <a href="{{ id.page_url }}">{{ id.title|default:_"Untitled" }}</a>
                </h3>
                {% if id|summary:160 as item_summary %}
                    <p class="content-list__summary">{{ item_summary }}</p>
                {% endif %}
            </div>

            {% if item_depiction %}
                <figure class="content-list__media">
                    {% image item_depiction mediaclass="home-list" alt=item_depiction.title|default:id.title %}
                </figure>
            {% endif %}
        </article>
    {% endwith %}
{% endif %}
