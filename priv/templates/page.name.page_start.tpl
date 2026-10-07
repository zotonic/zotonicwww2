{% extends "page.tpl" %}

{% block content_after %}
    <div class="page-relations category-index start-guides">
        <section class="category-index__section" aria-labelledby="start-guides-title">
            <header>
                <p class="category-index__eyebrow">{_ Documentation _}</p>
                <h2 id="start-guides-title">{_ Choose your guide _}</h2>
                <p>{_ Write and publish content, build a website, or keep an installation running. Choose your starting point. _}</p>
            </header>

            <div class="category-index__grid start-guides__grid">
                {% for guide in id.o.haspart|is_visible %}
                    {% include "_start_guide_card.tpl" id=guide %}
                {% endfor %}
            </div>
        </section>
    </div>
{% endblock %}
