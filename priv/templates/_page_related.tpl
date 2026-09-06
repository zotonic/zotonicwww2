{% if id.o.subject[1] as subject_id %}
    {% with m.search.query::%{
        cat: "text",
        hasobject: [subject_id, "subject"],
        id_exclude: id,
        is_published: true,
        sort: "-modified",
        pagelen: 6
    } as related %}
        {% if related %}
            <section class="related-content" aria-labelledby="related-content-title">
                <h2 id="related-content-title">{_ Related documentation _}</h2>
                <div class="related-content__grid">
                    {% for related_id in related %}
                        <article>
                            <p class="related-content__type">{{ related_id.category_id.title }}</p>
                            <h3><a href="{{ related_id.page_url }}">{{ related_id.title }}</a></h3>
                            <p>{{ related_id|summary:140 }}</p>
                        </article>
                    {% endfor %}
                </div>
            </section>
        {% endif %}
    {% endwith %}
{% endif %}
