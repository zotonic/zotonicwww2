<ul class="related-pages__grid">
    {% for target in targets %}
        <li>
            <a class="related-pages__card" href="{{ target.page_url }}">
                <span class="related-pages__text">
                    <strong>{{ target.title }}</strong>
                    {% with target|summary:160 as description %}
                        {% if description %}
                            <span class="related-pages__summary">{{ description }}</span>
                        {% endif %}
                    {% endwith %}
                </span>
                <span class="related-pages__arrow" aria-hidden="true">→</span>
            </a>
        </li>
    {% endfor %}
</ul>
