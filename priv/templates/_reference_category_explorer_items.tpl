{% include "_reference_result_items.tpl" result_ids=explorer.result_ids %}

{% if explorer.pager.next and not hide_loader %}
    <div id="reference-more-{{ explorer.pager.next }}"
         class="reference-result-list__loader"
         role="status"
         data-onvisible-topic="model/loadmore/post/replace"
         data-template="_reference_category_explorer_page.tpl"
         data-url="{% url none
            page=explorer.pager.next
            qs=explorer.query
            category=explorer.selected_category
            subject=explorer.subject
            module=explorer.module
            category_id=category_id
         %}">
        {_ Loading more documentation… _}
    </div>
{% endif %}
