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

        {# The home page body, for a short text with images expanded. #}
        {# In this text we explain all the pro's of using Zotonic     #}
        <div class="home__body">
            {{ id.body|show_media }}
        </div>

        {# Show a list with articles and page we want to highlight on the home page #}
        <div class="home__list">
            {% for id in id.o.haspart|is_visible %}
                {% if id.is_a.video %}
                    <div class="home__list__item{% if id.is_featured %} featured{% endif %}">
                        <figure class="fullwidth">
                            <div class="oembed-wrapper">
                                {% media id mediaclass=mediaclass %}
                            </div>
                        </figure>
                    </div>
                {% else %}
                    <div class="home__list__item{% if id.is_featured %} featured{% endif %} do_clickable">
                        {% include "_body_media.tpl" id=id.o.depiction size='large' %}
                        <h2><a href="{{ id.page_url }}">{{ id.title }}</a></h2>
                        <p>
                            {{ id|summary:240 }}
                        </p>
                    </div>
                {% endif %}
            {% endfor %}
        </div>

    </article>

{% endblock %}
