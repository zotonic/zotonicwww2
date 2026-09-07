{% extends "page.documentation.tpl" %}

{% block content %}
    {% if q.cluster and id.name != 'keyword' and not id.subject_topic_facet_category %}
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

{% if id.name == 'keyword' or id.subject_topic_facet_category %}
    {% include "_subject_topic_category.tpl" id=id %}
{% elif not q.cluster %}
<div class="page-relations">

    {# Only show the Table of Contents if the sub-pages are documentation
     # pages that are not in the current category.
     # Examples are the notifications with sub-pages where notifications
     # are combined per kind.
     #}

    {% if id.o.haspart|is_visible as haspart %}
        <div class="content-list">
            {% for id in haspart %}
                {% catinclude "_list_item.tpl" id %}
            {% endfor %}
        </div>
    {% endif %}

    {% for s in id.s.haspart|is_visible %}
        {% with s.o.haspart|is_visible as siblings %}
        {% for p in s.o.haspart %}
            {% if p == id %}
                <p class="page-haspart">
                    {% if siblings[forloop.counter - 1] as prev %}
                        <a class="haspart__prev" href="{{ prev.page_url }}">{{ prev.title }}</a>
                    {% else %}
                        <span></span>
                    {% endif %}
                    <a class="haspart__link" href="{{ s.page_url }}">{{ s.title }}</a>
                    {% if siblings[forloop.counter + 1] as next %}
                        <a class="haspart__next" href="{{ next.page_url }}">{{ next.title }}</a>
                    {% endif %}
                </p>
            {% endif %}
        {% endfor %}
        {% endwith %}
    {% endfor %}

    {% if id.o.relation|is_visible as relo %}
        <div class="connections">
            <h3>{_ See more _}</h3>

            <div class="list-items">
                {% for id in relo %}
                    {% catinclude "_list_item.tpl" id %}
                {% endfor %}
            </div>
        </div>
    {% endif %}

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
            {% with ((m.category[id].is_a.documentation
                      and id.name != 'releasenotes')
                     or id.name == 'category')
                    | if : "pivot_title"
                         : "-publication_start" as sort
            %}
                {% with m.search.paged[{query cat=id sort=sort pagelen=100 page=q.page}] as result %}
                    <div class="connections paged" id="content-pager">
                        <h3>
                            {_ All _} <span>{{ id.title }}</span>
                        </h3>
                        <div class="list-items">
                            {% for id in result %}
                                {% catinclude "_list_item.tpl" id %}
                            {% endfor %}
                        </div>

                        {% pager result=result id=id qargs hide_single_page %}
                    </div>
                {% endwith %}
            {% endwith %}
        {% endif %}
    {% endwith %}
</div>
{% endif %}

{% endblock %}
