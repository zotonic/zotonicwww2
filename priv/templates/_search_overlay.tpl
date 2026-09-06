<div id="site-search-overlay" class="site-search-overlay" hidden>
    <button
        class="site-search-overlay__backdrop"
        type="button"
        data-search-close
        aria-label="{_ Close search _}"
    ></button>

    <section class="site-search-overlay__panel" aria-label="{_ Live search results _}">
        <div class="site-search-overlay__toolbar">
            <p>{_ Live search _}</p>
            <button class="site-search-overlay__close" type="button" data-search-close>
                <span aria-hidden="true">&times;</span>
                <span class="sr-only">{_ Close search _}</span>
            </button>
        </div>

        <div
            id="site-search-overlay-results"
            class="search-feedback search-feedback--overlay do_feedback"
            data-feedback='{ "trigger": "site-search-form", "template": "_search_overlay_results.tpl", "timeout": 250 }'
            aria-live="polite"
            aria-busy="false"
        >
            {% include "_search_overlay_results.tpl" %}
        </div>
    </section>
</div>
