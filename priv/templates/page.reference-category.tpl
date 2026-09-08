{% extends "page.documentation.tpl" seo_noindex=q.cluster seo_follow=q.cluster %}

{# Shared by the named reference-category pages. Keeping this separate from
 # page.category.tpl preserves the dedicated keyword taxonomy pages. #}
{% block content %}
    {% if q.cluster %}
        {% with m.zotonicwww2_search.category_cluster::%{
                category: id,
                cluster: q.cluster,
                limit: 40
            }
            as cluster_view
        %}
            {% if cluster_view.active %}
                <article class="cluster-category-heading">
                    <h1>{{ id.title }}</h1>
                    <a class="cluster-category-heading__up"
                       href="{% if cluster_view.parent_path_value %}{% url none cluster=cluster_view.parent_path_value %}{% else %}{{ id.page_url }}{% endif %}">
                        <span aria-hidden="true">↑</span>
                        {_ Up to _}
                        {% if cluster_view.parent_is_other %}{_ Other _}{% else %}{{ cluster_view.parent_keyword.title|default:id.title }}{% endif %}
                    </a>
                </article>
                <div class="page-relations reference-category-page reference-category-page--cluster">
                    {% include "_category_cluster_view.tpl"
                        cluster_view=cluster_view
                        category_id=id
                    %}
                </div>
            {% else %}
                {% inherit %}
            {% endif %}
        {% endwith %}
    {% else %}
        {% inherit %}
    {% endif %}
{% endblock %}

{% block content_after %}
    {% if not q.cluster %}
    <div class="page-relations reference-category-page">
        {% block reference_category_before %}{% endblock %}
        {% with m.zotonicwww2_search.category_cluster::%{
                category: id,
                cluster: q.cluster,
                limit: 40
            }
            as cluster_view
        %}
            {% include "_category_cluster_view.tpl"
                cluster_view=cluster_view
                category_id=id
            %}

            {% if not cluster_view.active %}
                <div id="{{ #category_explorer }}"></div>
                {% live
                    topic="model/location/event/qlist"
                    template="_reference_category_explorer.tpl"
                    target=#category_explorer
                    method="patch"
                    category_id=id
                    is_new_query
                %}
            {% endif %}
        {% endwith %}
    </div>
    {% endif %}
{% endblock %}
