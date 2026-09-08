{% with m.zotonicwww2_search.results_page::%{
        text: q.qs,
        category: q.category,
        subject: q.subject,
        module: q.module,
        page: q.page,
        limit: 20
    }
    as search
%}
    {% include "_search_results_items.tpl" search=search %}
{% endwith %}
