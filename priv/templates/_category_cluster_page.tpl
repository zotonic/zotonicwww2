{% with m.rsc[q.category_id].id as category_id %}
    {% if category_id %}
        {% with m.zotonicwww2_search.category_cluster::%{
            category: category_id,
            cluster: q.cluster,
            page: q.page,
                limit: 40
            }
            as cluster_view
        %}
            {% include "_reference_result_items.tpl" result_ids=cluster_view.result_ids %}

            {% if cluster_view.pager.next %}
                <div id="category-cluster-more-{{ cluster_view.pager.next }}"
                     class="reference-result-list__loader"
                     role="status"
                     data-onvisible-topic="model/loadmore/post/replace"
                     data-template="_category_cluster_page.tpl"
                     data-url="{% url none
                        page=cluster_view.pager.next
                        cluster=cluster_view.path_value
                        category_id=category_id
                     %}">
                    {_ Loading more documentation… _}
                </div>
            {% endif %}
        {% endwith %}
    {% endif %}
{% endwith %}
