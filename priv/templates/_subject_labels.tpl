{% if subjects %}
    <ul class="subject-label-list{% if is_meta %} subject-label-list--meta{% endif %}" aria-label="{_ Subjects _}">
        {% for subject_id in subjects %}
            <li>{% include "_subject_label.tpl" subject_id=subject_id %}</li>
        {% endfor %}
    </ul>
{% endif %}
