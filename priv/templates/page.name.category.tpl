{% extends "page.documentation.tpl" %}

{# The category resource is itself the index of all categories. Put the
 # documentation taxonomy first; the remaining site categories follow below. #}
{% block content_after %}
    {% with [
        'documentation',
        'userguide',
        'developerguide',
        'cookbook',
        'article',
        'reference',
        'module',
        'controller',
        'model',
        'dispatch',
        'template_tag',
        'template_filter',
        'action',
        'template_action',
        'template_scomp',
        'validator',
        'template_validator',
        'template',
        'notification',
        'releasenotes'
    ] as documentation_category_names %}
        <div class="page-relations category-index">
            <section class="category-index__section" aria-labelledby="documentation-categories-title">
                <header>
                    <p class="category-index__eyebrow">{_ Documentation structure _}</p>
                    <h2 id="documentation-categories-title">{_ Documentation categories _}</h2>
                    <p>{_ Browse guides and reference material by their primary type. Reference categories open an automatically updated keyword explorer. _}</p>
                </header>
                <div class="category-index__grid">
                    {% for category_name in documentation_category_names %}
                        {% if m.rsc[category_name].id as category_id %}
                            {% with m.search.paged[{query cat=category_id pagelen=1}] as category_result %}
                                <article class="category-index__card">
                                    <a href="{{ category_id.page_url }}">
                                        <h3>{{ category_id.title }}</h3>
                                        {% if category_id.summary %}
                                            <p>{{ category_id.summary }}</p>
                                        {% endif %}
                                        <span>
                                            {{ category_result.total }}
                                            {% if category_result.total == 1 %}{_ item _}{% else %}{_ items _}{% endif %}
                                        </span>
                                    </a>
                                </article>
                            {% endwith %}
                        {% endif %}
                    {% endfor %}
                </div>
            </section>

            {% with m.search[{query cat=id sort=`pivot_title` pagelen=500}] as all_categories %}
                <section class="category-index__section" aria-labelledby="other-categories-title">
                    <header>
                        <p class="category-index__eyebrow">{_ Site taxonomy _}</p>
                        <h2 id="other-categories-title">{_ Other categories _}</h2>
                    </header>
                    <ul class="category-index__links">
                        {% for category_id in all_categories %}
                            {% if not category_id.name|member:documentation_category_names %}
                                <li><a href="{{ category_id.page_url }}">{{ category_id.title }}</a></li>
                            {% endif %}
                        {% endfor %}
                    </ul>
                </section>
            {% endwith %}
        </div>
    {% endwith %}
{% endblock %}
