{% if cluster_view.active or cluster_view.clusters %}
    <section class="category-clusters{% if cluster_view.active %} category-clusters--active{% endif %}"
             aria-labelledby="{{ #category_clusters_title }}">
        <header class="category-clusters__header">
            <div>
                <p class="category-clusters__eyebrow">
                    {% if cluster_view.active %}{_ Topic cluster _}{% else %}{_ Browse by topic _}{% endif %}
                </p>
                <h2 id="{{ #category_clusters_title }}">
                    {% if cluster_view.active %}
                        {% if cluster_view.is_other %}
                            {_ Other _}
                        {% else %}
                            {{ cluster_view.current_keyword.title }}
                        {% endif %}
                    {% else %}
                        {_ Discover _} {{ cluster_root_id.title|default:category_id.title }}
                    {% endif %}
                </h2>
                {% if cluster_view.is_other %}
                    <p>{_ Documentation not covered by the keyword groups shown in the parent cluster. _}</p>
                {% elif cluster_view.active and cluster_view.current_keyword.summary %}
                    <p>{{ cluster_view.current_keyword.summary }}</p>
                {% elif cluster_view.clusters %}
                    <p>{_ Explore related documentation through its most useful keyword groups. _}</p>
                {% endif %}
            </div>
            <p class="category-clusters__count">
                {{ cluster_view.total }}
                {% if cluster_view.total == 1 %}{_ item _}{% else %}{_ items _}{% endif %}
            </p>
        </header>

        {% if cluster_view.clusters %}
            <div class="category-clusters__split">
                <p>
                    {_ Grouped by _}
                    <strong>{{ cluster_view.cluster_facet.title }}</strong>
                </p>
                <div class="category-cluster-grid">
                    {% for cluster in cluster_view.clusters %}
                        <article class="category-cluster-card">
                            <header>
                                <h3>
                                    <a class="category-cluster-card__link"
                                       href="{% url none cluster=cluster.path_value %}">
                                        {% if cluster.is_other %}{_ Other _}{% else %}{{ cluster.keyword_id.title }}{% endif %}
                                    </a>
                                </h3>
                                <span>
                                    {{ cluster.total }}
                                    {% if cluster.total == 1 %}{_ item _}{% else %}{_ items _}{% endif %}
                                </span>
                            </header>
                            {% if cluster.is_other %}
                                <p class="category-cluster-card__summary">
                                    {_ Documentation not covered by the other keyword groups in this split. _}
                                </p>
                            {% elif cluster.keyword_id.summary %}
                                <p class="category-cluster-card__summary">{{ cluster.keyword_id.summary }}</p>
                            {% endif %}
                            {% if cluster.keywords %}
                                <ul class="subject-label-list category-cluster-card__keywords" aria-label="{_ Important keywords _}">
                                    {% for subject_id in cluster.keywords %}
                                        <li>{% include "_subject_label.tpl" subject_id=subject_id %}</li>
                                    {% endfor %}
                                </ul>
                            {% endif %}
                            <ul class="category-cluster-card__resources">
                                {% for result_id in cluster.result_ids %}
                                    <li><a href="{{ result_id.page_url }}">{{ result_id.title|default:_"Untitled" }}</a></li>
                                {% endfor %}
                            </ul>
                            <a class="category-cluster-card__more"
                               href="{% url none cluster=cluster.path_value %}">
                                {_ Explore this cluster _} <span aria-hidden="true">→</span>
                            </a>
                        </article>
                    {% endfor %}
                </div>
            </div>
        {% elif cluster_view.active %}
            <div class="category-clusters__results">
                <p class="category-clusters__leaf-note">
                    {% if cluster_view.is_other %}
                        {_ All documentation outside the keyword groups in the parent cluster is listed below. _}
                    {% else %}
                        {_ The remaining keywords do not make a useful further split. All matching documentation is listed below. _}
                    {% endif %}
                </p>
                {% if cluster_view.result_ids %}
                    <div class="reference-result-list" aria-live="polite">
                        {% include "_reference_result_items.tpl" result_ids=cluster_view.result_ids %}
                        {% if cluster_view.pager.next %}
                            <div id="category-cluster-more-{{ cluster_view.pager.next }}"
                                 class="reference-result-list__loader"
                                 role="status"
                                 data-onvisible-topic="model/loadmore/post/replace"
                                 data-template="{{ cluster_page_template|default:"_category_cluster_page.tpl"|escape }}"
                                 {% if keyword_id %}
                                 data-url="{% url none
                                    page=cluster_view.pager.next
                                    cluster=cluster_view.path_value
                                    keyword_id=keyword_id
                                 %}"
                                 {% else %}
                                 data-url="{% url none
                                    page=cluster_view.pager.next
                                    cluster=cluster_view.path_value
                                    category_id=category_id
                                 %}"
                                 {% endif %}>
                                {_ Loading more documentation… _}
                            </div>
                        {% endif %}
                    </div>
                {% else %}
                    <div class="reference-explorer__empty">
                        <h3>{_ No matching documentation _}</h3>
                    </div>
                {% endif %}
            </div>
        {% endif %}
    </section>
{% endif %}
