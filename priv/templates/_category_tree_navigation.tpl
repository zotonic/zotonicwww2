{% with m.category[id].parent_id as parent_id %}
{% with m.category[id].tree1 as subcategories %}
    {% if parent_id or subcategories %}
        <nav class="category-tree" aria-labelledby="{{ #category_tree_title }}">
            <header class="category-tree__header">
                <p class="category-tree__eyebrow">{_ Category hierarchy _}</p>
                <h2 id="{{ #category_tree_title }}">{_ Browse nearby categories _}</h2>
            </header>

            <div class="category-tree__groups">
                {% if parent_id and parent_id.is_visible %}
                    <section class="category-tree__group category-tree__group--parent"
                             aria-labelledby="{{ #parent_category_title }}">
                        <h3 id="{{ #parent_category_title }}">{_ Parent category _}</h3>
                        <a class="category-tree__link category-tree__link--parent"
                           href="{{ parent_id.page_url }}">
                            <span class="category-tree__direction" aria-hidden="true">↑</span>
                            <span class="category-tree__copy">
                                <strong>{{ parent_id.title }}</strong>
                                {% if parent_id.summary %}
                                    <span>{{ parent_id.summary }}</span>
                                {% endif %}
                            </span>
                        </a>
                    </section>
                {% endif %}

                {% if subcategories %}
                    <section class="category-tree__group category-tree__group--children"
                             aria-labelledby="{{ #subcategories_title }}">
                        <h3 id="{{ #subcategories_title }}">{_ Subcategories _}</h3>
                        <ul class="category-tree__children">
                            {% for child in subcategories %}
                                {% if child.id.is_visible %}
                                    <li>
                                        <a class="category-tree__link" href="{{ child.id.page_url }}">
                                            <span class="category-tree__copy">
                                                <strong>{{ child.id.title }}</strong>
                                                {% if child.id.summary %}
                                                    <span>{{ child.id.summary }}</span>
                                                {% endif %}
                                            </span>
                                            <span class="category-tree__direction" aria-hidden="true">→</span>
                                        </a>
                                    </li>
                                {% endif %}
                            {% endfor %}
                        </ul>
                    </section>
                {% endif %}
            </div>
        </nav>
    {% endif %}
{% endwith %}
{% endwith %}
