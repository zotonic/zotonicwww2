{% with m.zotonicwww2_search.results::%{
        text: q.qs,
        category: q.category,
        subject: q.subject,
        module: q.module,
        limit: 12
    }
    as search
%}
    {% include "_search_results_content.tpl"
        search=search
        search_form_id="site-search-form"
        is_overlay
    %}
{% endwith %}
