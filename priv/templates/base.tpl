<!DOCTYPE html>

<!--
  This is the page template that is the basis for all other page templates.
  The base template MUST be called 'base.tpl'.
  It defines the basic layout, and includes the common css and javascript.
-->

<!-- The current language is available in the variable 'z_language' -->
<html xml:lang="{{ z_language }}">

  <head>
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1, maximum-scale=1, user-scalable=0">

    <!-- A block is defined to allow other templates to set their own title -->
    <!-- If not on the home page, include the page's title -->
    <title>
      {% block title %}
        {% if id and id.name != 'page_home' %}{{ id.title }} @ {% endif %} {{ m.site.title }}
      {% endblock %}
    </title>

    <!--
      The lib tag combines css or js files into a single link or script element
      All lib files can be found in the "priv/lib/" folders of active modules
      or sites.
    -->
    {% lib
          "css/bs3.css"
          "css/site.css"
          "css/logon.css"
          "font-awesome-4/css/font-awesome.css"
    %}

    <!--
      The following "all include" fetches templates from modules into the <head/>
      Examples are Google Analytics, Atom feeds, and SEO meta tags.
      Convention is to start included templates with a '_', this to make
      them different from page templates that start with a letter.
    -->
    {% all include "_html_head.tpl" %}

    <!--
      Convention is to add a block 'html_head_extra' so that standard pages
      like the logon page can inject some extra elements into the <head/> section.
    -->
    {% block html_head_extra %}{% endblock %}
  </head>

  <!--
    It is a convention to add the current page's name and its categories as
    classes to the body tag. This allows easier styling of category specific pages.
    The 'body_class' and 'body_attrs' block allows to add your extra classes
    or attributes to the body tag.
    The 'data-cotonic-pathname-search' is used by the Cotonic UI components

    'id.name' is the unique name of the resource being displayed, it can be undefined.
    'is_a' is a list of the category and parent categories this page belongs to, the most
    specific category is mentioned last.
  -->
  <body class="{% block body_class %}page-{{ id.name }}{% endblock %} {% for cat in id.is_a %}cat-{{ cat }} {% endfor %}"
        {% block body_attrs %}{% endblock %}
        data-cotonic-pathname-search="{% cotonic_pathname_search %}">

    <!-- Top of page header -->
    <div id="header-wrapper">
      <header id="header">
        {% include "_nav_header.tpl" %}
      </header>
    </div>

    <!-- Optional page header -->
    {% block page_header %}{% endblock %}

    <!-- Main navigation sidebar -->
    <input type="checkbox" id="nav-toggle" />
    <label id="sidedrawer-bg" for="nav-toggle"></label>
    <nav id="sidedrawer">
      <div>
        {% include "_nav_sidedrawer.tpl" %}
      </div>
    </nav>

    <!-- Main content including footer -->
    <div id="content-wrapper">

      <!--
        Convention is to give the element that contains the page content
        the id 'content'
      -->
      <main id="content">
        <!--
          There MUST be a 'content_area' block where pages like
          the logon.tpl can place their content. This should maximize
          the available space.
        -->
        {% block content_area %}
          <!-- This block is only for this site -->
          {% block content_before %}
          {% endblock %}
          <!--
            There MUST be a 'content' block for pages to place
            content that don't need to have maximum available space
          -->
          {% block content %}
          {% endblock %}
          <!-- This block is only for this site -->
          {% block content_after %}
          {% endblock %}
          <!--
            The block 'below_body' is used by some modules to add
            content, it MUST be present to allow those modules to
            work properly.
            Example is 'mod_survey' which adds buttons for survey pages.
          -->
          {% block below_body %}
          {% endblock %}
        {% endblock %}
      </main>

      <!-- Convention is to wrap the page footer in a block 'footer' -->
      {% block footer %}
        {% include "_nav_footer.tpl" %}
      {% endblock %}

    </div>

    <!--
      Like the '_html_head.tpl' above, this allows modules to add
      code and/or markup to the end of the body element.
      It MUST be present for modules like mod_auth2fa
    -->
    {% all include "_html_body.tpl" %}

    <!--
      Include a template that includes all JavaScript files.
      Convention is to call this template '_js_include.tpl'
    -->
    {% include "_js_include.tpl" %}

    <!--
      The 'script' tag places all the JavaScript collected from the
      the 'javascript' and other tags in the templates. It also inlines
      the code to start the server/client websocket bridge.
    -->
    {% script %}

  </body>

</html>
