<nav class="site-nav" aria-label="{_ Main navigation _}">
    <a class="site-nav__brand" href="{% url home %}" aria-label="{_ Zotonic home _}">
        <img
            src="{% image_url '/lib/images/zotonic/zotonic-logo.png' mediaclass='logo' %}"
            alt="{{ m.site.title }}"
        >
    </a>

    <ul class="site-nav__links">
        <li><a href="{{ m.rsc.userguide.page_url }}">{_ Start _}</a></li>
        <li><a href="{{ m.rsc.cookbook.page_url }}">{_ Guides _}</a></li>
        <li><a href="{{ m.rsc.reference.page_url }}">{_ Reference _}</a></li>
        <li><a href="{{ m.rsc.doc_releasenotes_index.page_url }}">{_ Releases _}</a></li>
    </ul>

    <form id="site-search-form" class="site-nav__search" action="{% url search %}" method="get" role="search">
        <label class="sr-only" for="site-search">{_ Search Zotonic documentation _}</label>
        <input
            id="site-search"
            name="qs"
            type="search"
            placeholder="{_ Search documentation _}"
            value="{% if q.qs %}{{ q.qs|escape }}{% endif %}"
            autocomplete="off"
            aria-controls="site-search-overlay"
            aria-expanded="false"
        >
        <input type="hidden" name="category" value="{{ q.category|escape }}">
        <input type="hidden" name="subject" value="{{ q.subject|escape }}">
        <input type="hidden" name="module" value="{{ q.module|escape }}">
        <button type="submit" aria-label="{_ Search _}">
            <span class="glyphicon glyphicon-search" aria-hidden="true"></span>
        </button>
    </form>
</nav>
