{#
    This is a special variation on the '_list_item.tpl' template.
    It is used for pages with category 'website'.
    And is included as:

        {% catinclude "_list_item.tpl" id %}

    Where 'id' contains the id of a page in category 'website' or a sub-category
#}
{% include "_content_list_item.tpl" id=id depiction=id is_highlight=is_highlight %}
