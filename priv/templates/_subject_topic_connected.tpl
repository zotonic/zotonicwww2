{% with m.rsc[topic_id].id as safe_topic_id %}
    {% with m.search.paged[{query hasobject=[safe_topic_id, `subject`] sort=`pivot_title` pagelen=12}] as result %}
        <div class="list-items" id="{{ #results }}">
            {% for connected_id in result %}
                {% catinclude "_list_item.tpl" connected_id %}
            {% empty %}
                <p class="text-muted">{_ No content is connected to this keyword yet. _}</p>
            {% endfor %}
        </div>

        {% lazy
            action={moreresults
                result=result
                target=#results
                template="_list_item.tpl"
                catinclude
                visible
            }
        %}
    {% endwith %}
{% endwith %}
