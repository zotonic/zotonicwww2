{% if guide.children %}
<section class="guide-contents" aria-labelledby="guide-contents-title">
    <h2 id="guide-contents-title">{% if guide.root == id %}{_ Explore this guide _}{% else %}{_ In this section _}{% endif %}</h2>
    <ol class="guide-contents__grid">
        {% for item in guide.children %}
            <li><a href="{{ item.page_url }}?guide={{ guide.root }}"><span class="guide-contents__number">{{ forloop.counter }}</span><span><strong>{{ item.title }}</strong>{% if item.summary %}<span class="guide-contents__summary">{{ item.summary }}</span>{% endif %}</span><span aria-hidden="true">→</span></a></li>
        {% endfor %}
    </ol>
</section>
{% endif %}
{% if guide.additional %}
<details class="guide-extra">
    <summary>{_ More articles and background _} ({{ guide.additional|length }})</summary>
    <ul>{% for item in guide.additional %}<li><a href="{{ item.page_url }}?guide={{ guide.root }}">{{ item.title }}</a></li>{% endfor %}</ul>
</details>
{% endif %}
{% if guide.parent %}
<nav class="guide-pagination" aria-label="{_ Reading order _}">
    {% if guide.previous %}<a href="{{ guide.previous.page_url }}?guide={{ guide.root }}" rel="prev"><small>{_ Previous _}</small><span>← {{ guide.previous.title }}</span></a>{% else %}<span></span>{% endif %}
    <a href="{{ guide.parent.page_url }}?guide={{ guide.root }}"><small>{_ Contents _}</small><span>{{ guide.parent.title }}</span></a>
    {% if guide.next %}<a href="{{ guide.next.page_url }}?guide={{ guide.root }}" rel="next"><small>{_ Next _}</small><span>{{ guide.next.title }} →</span></a>{% else %}<span></span>{% endif %}
</nav>
{% endif %}
{% if guide.alternatives %}<p class="guide-alternatives">{_ Also in _}: {% for parent in guide.alternatives %}<a href="{{ parent.page_url }}">{{ parent.title }}</a>{% if not forloop.last %} · {% endif %}{% endfor %}</p>{% endif %}
