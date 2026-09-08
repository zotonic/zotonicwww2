
{% lib
    "js/apps/jquery-latest.min.js"
    "js/apps/jquery-ui-latest.min.js"
    "js/modules/jquery.ui.touch-punch.min.js"
%}

{% lib
    "js/modules/jstz.min.js"
    "cotonic/cotonic.js"

    "js/models/loadmore.js"

    "js/apps/zotonic-wired.js"
    "js/apps/z.widgetmanager.js"
    "js/modules/z.forminit.js"
    "js/modules/z.live.js"
    "js/modules/z.notice.js"
    "js/modules/z.dialog.js"
    "js/modules/z.clickable.js"
    "js/modules/z.feedback.js"
    "js/modules/z.survey_test_feedback.js"
    "js/modules/livevalidation-1.3.js"
    "js/modules/jquery.loadmask.js"

    "bootstrap/js/bootstrap.min.js"

    "js/zotonicwww2-search.js"

    minify
%}

{% worker name="auth" src="js/zotonic.auth.worker.js" args=%{  auth: m.authentication.status  } %}

{% block _js_include_extra %}{% endblock %}

{# Scroll-state detection is disabled while the header behavior is being redesigned. #}

<script type="text/javascript" nonce="{{ m.req.csp_nonce }}">
    $(function()
    {
        $.widgetManager();
    });
</script>
