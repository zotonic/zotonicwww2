{#
    This is a special variation on the '_list_item.tpl' template.
    It is used for pages with category 'website'.
    And is included as:

        {% catinclude "_list_item.tpl" id %}

    Where 'id' contains the id of a page in category 'website' or a sub-category
#}
<div class="list-item list-item-website {% if is_highlight or id.is_featured %} featured{% endif %} do_clickable">
    {% image id mediaclass="list-item" %}
    <h3>
        <a href="{{ id.page_url }}">
            {{ id.title }}
            <span class="text-muted">{{ id.category_id.title|lower }}</span>
        </a>
    </h3>
</div>
