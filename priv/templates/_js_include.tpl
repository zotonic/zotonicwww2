
{% include "_js_include_jquery.tpl" %}

{% lib
    "js/modules/jstz.min.js"
    "cotonic/zotonic-wired-bundle.js"
    "js/apps/zotonic-wired.js"
    "js/apps/z.widgetmanager.js"
    "js/modules/z.notice.js"
    "js/modules/z.dialog.js"
    "js/modules/z.clickable.js"
    "js/modules/livevalidation-1.3.js"
    "js/modules/jquery.loadmask.js"
    "bootstrap/js/bootstrap.min.js"
%}

{% worker name="auth" src="js/zotonic.auth.worker.js" %}

{% block _js_include_extra %}{% endblock %}

<script type="text/javascript">
  $(function() {
    var lastScrollTop = 0;
    var isScrolledDown = false;

    window.onscroll = function() {
      var scrollTop = $(document).scrollTop();
      if (scrollTop > lastScrollTop) {
        if (!isScrolledDown && scrollTop > 50) {
          $('body').addClass('scrolled-down');
          isScrolledDown = true;
        }
      } else if (isScrolledDown) {
        $('body').removeClass('scrolled-down');
        isScrolledDown = false;
      }
      lastScrollTop = scrollTop;
    }
  });
</script>

<script type="text/javascript">
    $(function()
    {
        $.widgetManager();
    });
</script>
