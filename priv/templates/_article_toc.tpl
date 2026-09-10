{% if toc %}
    <nav class="article-toc" aria-label="{_ On this page _}">
        <p class="article-toc__title">{_ On this page _}</p>
        {% include "page-parts/_toc.tpl" toc=toc %}
    </nav>
{% endif %}
