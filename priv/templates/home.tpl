{#
    This is the template for the home page.

    It is selected by the dispatch rule (see priv/dispatch/dispatch):

    {home, [], controller_page, [ {template, "home.tpl"}, {id, page_home} ]}

    This selects the resource with unique name "page_home", and displays it
    using the template "home.tpl".

    Normally the controller_page would use "page.tpl", but as the home page is
    special we use "home.tpl", which is also a convention normally used for
    Zotonic sites.

    Note that the resource page_home has its "page_path" property set to "/".
    This matches with the the empty ([]) path of the 'home' dispatch rule.
#}
{% extends "base.tpl" %}

{% block title %}{{ m.site.title }}{% endblock %}

{% block content %}

    <article>
        {% if id.summary or id.body %}
            <header class="home-intro">
                {% if id.summary %}
                    <h1 class="home__summary">{{ id.summary }}</h1>
                {% endif %}

                {% if id.body %}
                    <div class="home__body">
                        {{ id.body|show_media }}
                    </div>
                {% endif %}
            </header>
        {% endif %}

        {# Show the pages connected to the home page as a balanced feature grid. #}
        <div class="home__featured">
            {% for id in id.o.haspart %}
                {% with id.depiction as depiction %}
                    <article
                        class="home__featured__item{% if not depiction %} home__featured__item--text{% endif %} do_clickable"
                        aria-labelledby="home-featured-{{ id }}"
                    >
                        {% if depiction %}
                            <figure class="home__featured__media">
                                {% image depiction mediaclass="home-featured" alt=depiction.title|default:id.title %}
                            </figure>
                        {% endif %}
                        <div class="home__featured__body">
                            <h2 id="home-featured-{{ id }}">
                                <a href="{{ id.page_url }}">{{ id.title }}</a>
                            </h2>
                            <p>{{ id|summary:120 }}</p>
                        </div>
                    </article>
                {% endwith %}
            {% endfor %}
        </div>

        {# A compact editorial index of recent releases and team-written content. #}
        <div class="home__list">
            <section class="home-feed home-feed--releases" aria-labelledby="home-releases-title">
                <header class="home-feed__header">
                    <div>
                        <h2 id="home-releases-title">{_ Latest releases _}</h2>
                        <p>{_ What changed, what was fixed, and how to upgrade. _}</p>
                    </div>
                    <a class="home-feed__more" href="{{ m.rsc.doc_releasenotes_index.page_url }}">
                        {_ All release notes _}
                        <svg viewBox="0 0 16 16" aria-hidden="true">
                            <path d="M3 8h9M8.5 4.5 12 8l-3.5 3.5" />
                        </svg>
                    </a>
                </header>

                <div class="home-release-list">
                    {% for id in m.search.query::%{
                            cat: [ "releasenotes" ],
                            is_published: true,
                            sort: [ "-is_featured", "-created" ],
                            pagelen: 2,
                            page: 1
                        }
                    %}
                        <article class="home-release-list__item do_clickable">
                            <p class="home-entry__type">
                                <span>{_ Release notes _}</span>
                                {% if id.org_pubdate %}
                                    <time datetime="{{ id.org_pubdate|date:"c":"UTC" }}">
                                        {{ id.org_pubdate|date:_"j M Y":"UTC" }}
                                    </time>
                                {% endif %}
                            </p>
                            <h3><a href="{{ id.page_url }}">{{ id.title }}</a></h3>
                            <p class="home-entry__summary">{{ id|summary:180 }}</p>
                        </article>
                    {% endfor %}
                </div>
            </section>

            <section class="home-feed home-feed--reading" aria-labelledby="home-reading-title">
                <header class="home-feed__header">
                    <div>
                        <h2 id="home-reading-title">{_ Articles and cookbook recipes _}</h2>
                        <p>{_ Practical explanations and solutions from the Zotonic team. _}</p>
                    </div>
                </header>

                <div class="home-reading-list">
                    {% for id in m.search.query::%{
                            cat: [ "article", "cookbook" ],
                            is_published: true,
                            sort: [ "-is_featured", "-publication_start" ],
                            pagelen: 8,
                            page: 1
                        }
                    %}
                        {% with id.depiction as depiction %}
                            <article class="home-reading-list__item{% if depiction %} home-reading-list__item--media{% else %} home-reading-list__item--text{% endif %} do_clickable">
                                <p class="home-reading-list__meta">
                                    <span>{{ id.category_id.title }}</span>
                                    {% if id.is_a.article and id.publication_start %}
                                        <time datetime="{{ id.publication_start|date:"c" }}">
                                            {{ id.publication_start|date:_"j M Y" }}
                                        </time>
                                    {% endif %}
                                </p>
                                <div class="home-reading-list__copy">
                                    <h3><a href="{{ id.page_url }}">{{ id.title }}</a></h3>
                                    {% if id|summary as item_summary %}
                                        <p>{{ item_summary|truncate:220 }}</p>
                                    {% endif %}
                                </div>
                                {% if depiction %}
                                    <figure class="home-reading-list__media">
                                        {% image depiction mediaclass="home-list" alt=depiction.title|default:id.title %}
                                    </figure>
                                {% endif %}
                            </article>
                        {% endwith %}
                    {% endfor %}
                </div>
            </section>
        </div>

    </article>

{% endblock %}
