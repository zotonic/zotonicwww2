{% extends "page.tpl" %}

{% block content_after %}
    <div class="page-relations category-index start-guides">
        <section class="category-index__section" aria-labelledby="start-guides-title">
            <header>
                <p class="category-index__eyebrow">{_ Documentation _}</p>
                <h2 id="start-guides-title">{_ Choose your guide _}</h2>
                <p>{_ Learn how to use Zotonic as a content editor, or dive into building and extending Zotonic sites. _}</p>
            </header>

            <div class="category-index__grid start-guides__grid">
                {% include "_start_guide_card.tpl"
                    id=m.rsc.doc_userguide_index.id
                    description=_"Learn how to manage content, users, and sites with Zotonic."
                %}
                {% include "_start_guide_card.tpl"
                    id=m.rsc.doc_developerguide_index.id
                    description=_"Build and extend Zotonic sites, from setup and structure to deployment."
                %}
            </div>
        </section>
    </div>
{% endblock %}
