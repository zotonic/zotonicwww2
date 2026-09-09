{% with m.zotonicwww2_search.results::%{
        text: q.qs,
        category: q.category,
        subject: q.subject,
        module: q.module,
        page: q.page,
        limit: 20
    }
    as search
%}
    {% include "_search_results_content.tpl"
        search=search
        search_form_id="search-form"
        is_live_query
    %}
{% endwith %}
