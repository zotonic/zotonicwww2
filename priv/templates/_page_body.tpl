{% with id.body|toc:4 as toc, body %}
    {% include "_article_toc.tpl" toc=toc %}

    <div class="body">
        {{ body|show_media }}
    </div>
{% endwith %}
