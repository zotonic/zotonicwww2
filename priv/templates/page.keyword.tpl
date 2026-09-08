{% extends "page.tpl" seo_noindex=q.cluster seo_follow=q.cluster %}

{% block content %}
    {% if q.cluster %}
        {% with m.zotonicwww2_search.category_cluster::%{
                subject: id,
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
                <div class="page-relations subject-topic-page subject-topic-page--cluster">
                    {% include "_category_cluster_view.tpl"
                        cluster_view=cluster_view
                        cluster_root_id=id
                        keyword_id=id
                        cluster_page_template="_keyword_cluster_page.tpl"
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
<div class="page-relations subject-topic-page">
    {% with id.o.subject_topic_broader|is_visible as broader_topics %}
    {% with id.s.subject_topic_broader|is_visible as narrower_topics %}
    {% with id.o.relation|is_visible as related_out %}
    {% with id.s.relation|is_visible as related_in %}
        {% if broader_topics or narrower_topics or related_out or related_in %}
            <section class="connections" aria-labelledby="subject-related-title">
                <h2 id="subject-related-title">{_ Related concepts _}</h2>
                <ul class="subject-label-list">
                    {% for topic_id in broader_topics %}
                        <li>
                            {% include "_subject_label.tpl" subject_id=topic_id %}
                            <span class="subject-label-relation">{_ broader concept _}</span>
                        </li>
                    {% endfor %}
                    {% for topic_id in narrower_topics %}
                        <li>
                            {% include "_subject_label.tpl" subject_id=topic_id %}
                            <span class="subject-label-relation">{_ narrower concept _}</span>
                        </li>
                    {% endfor %}
                    {% for topic_id in related_out %}
                        {% if not topic_id|member:broader_topics and not topic_id|member:narrower_topics %}
                            <li>{% include "_subject_label.tpl" subject_id=topic_id %}</li>
                        {% endif %}
                    {% endfor %}
                    {% for topic_id in related_in %}
                        {% if not topic_id|member:broader_topics and not topic_id|member:narrower_topics and not topic_id|member:related_out %}
                            <li>{% include "_subject_label.tpl" subject_id=topic_id %}</li>
                        {% endif %}
                    {% endfor %}
                </ul>
            </section>
        {% endif %}
    {% endwith %}
    {% endwith %}
    {% endwith %}
    {% endwith %}

    {% if id.subject_topic_aliases or id.subject_topic_source_url or id.subject_topic_wikidata_id %}
        <section class="connections" aria-labelledby="subject-details-title">
            <h2 id="subject-details-title">{_ Concept details _}</h2>
            <dl>
                {% if id.subject_topic_aliases %}
                    <dt>{_ Also known as _}</dt>
                    <dd>
                        {% for alias in id.subject_topic_aliases %}
                            {{ alias }}{% if not forloop.last %}, {% endif %}
                        {% endfor %}
                    </dd>
                {% endif %}
                {% if id.subject_topic_source_url %}
                    <dt>{_ Source _}</dt>
                    <dd>
                        <a href="{{ id.subject_topic_source_url|sanitize_url }}" rel="external noopener">
                            {{ id.subject_topic_source_url|escape }}
                        </a>
                    </dd>
                {% endif %}
                {% if id.subject_topic_wikidata_id %}
                    <dt>{_ Wikidata _}</dt>
                    <dd>
                        <a href="https://www.wikidata.org/wiki/{{ id.subject_topic_wikidata_id|escape }}" rel="external noopener">
                            {{ id.subject_topic_wikidata_label|default:id.subject_topic_wikidata_id }}
                        </a>
                    </dd>
                {% endif %}
            </dl>
        </section>
    {% endif %}

    {% with m.zotonicwww2_search.category_cluster::%{
            subject: id,
            limit: 40
        }
        as cluster_view
    %}
        {% include "_category_cluster_view.tpl"
            cluster_view=cluster_view
            cluster_root_id=id
            keyword_id=id
            cluster_page_template="_keyword_cluster_page.tpl"
        %}
    {% endwith %}

    <section class="connections" aria-labelledby="subject-connected-title">
        <h2 id="subject-connected-title">{_ Connected content _}</h2>
        <div id="{{ #connected }}">
            {% lazy
                template="_subject_topic_connected.tpl"
                target=#connected
                topic_id=id
            %}
        </div>
    </section>
</div>
{% endif %}
{% endblock %}
