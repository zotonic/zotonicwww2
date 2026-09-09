{% with m.search[{query cat=facet_id sort=`pivot_title` pagelen=500}] as subject_topics %}
    <ul class="subject-label-list" aria-label="{_ Keywords _}">
        {% for topic_id in subject_topics %}
            <li>{% include "_subject_label.tpl" subject_id=topic_id %}</li>
        {% empty %}
            <li class="text-muted">{_ No keywords in this group. _}</li>
        {% endfor %}
    </ul>
{% endwith %}
