{% with m.rsc[subject_id] as subject %}
    {% with subject.category_id.subject_topic_facet as subject_facet %}
        <a
            class="subject-label{% if subject_facet %} subject-label--{{ subject_facet|escape }}{% endif %}"
            href="{{ subject.page_url }}"
        >
            {{ subject.title }}
        </a>
    {% endwith %}
{% endwith %}
