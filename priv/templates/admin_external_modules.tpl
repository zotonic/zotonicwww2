{% extends "admin_base.tpl" %}

{% block title %}{_ External modules _}{% endblock %}

{% block content %}
{% if m.acl.is_admin %}
    <h2>{_ External module documentation _}</h2>
    <p>{_ Public HTTPS Git repositories are checked daily. Erlang module documentation and dispatch rules are imported; repository code is never compiled. _}</p>
    <p>{_ Refresh this page to see processing progress. Disabling a repository pauses updates and keeps its existing pages. _}</p>

    <p>{_ Deprecating a repository hides all its documentation and pauses updates. _}</p>
    <p>{% button text=_"Add repository" postback={new} delegate=`m_zotonicwww2_external` class="btn btn-primary" %}</p>

    <div class="table-responsive">
        <table class="table table-striped">
            <thead>
                <tr>
                    <th scope="col">{_ Repository _}</th>
                    <th scope="col">{_ Branch _}</th>
                    <th scope="col">{_ Added by _}</th>
                    <th scope="col">{_ Last fetched _} / {_ Last imported _}</th>
                    <th scope="col">{_ Status _}</th>
                    <th scope="col">{_ Pages _}</th>
                    <th scope="col">{_ Actions _}</th>
                </tr>
            </thead>
            <tbody>
                {% for repo in m.zotonicwww2_external.list %}
                    <tr>
                        <th scope="row">
                            {{ repo.title|escape }}<br>
                            <small>{{ repo.git_url|escape }}</small>
                            {% if repo.website_url %}<br><small>{{ repo.website_url|escape }}</small>{% endif %}
                            {% if repo.hex_package %}<br><small>{_ Hex package _}: {{ repo.hex_package|escape }}</small>{% endif %}
                        </th>
                        <td>{{ repo.branch|escape|default:_"Default" }}</td>
                        <td>
                            {% if repo.creator_id %}
                                <a href="{% url admin_edit_rsc id=repo.creator_id %}">{{ m.rsc[repo.creator_id].title|default:_"Unknown" }}</a>
                            {% else %}
                                {_ Unknown _}
                            {% endif %}
                            <br><small>{{ repo.created|date:"Y-m-d H:i" }}</small>
                        </td>
                        <td>
                            {{ repo.last_fetched|date:"Y-m-d H:i"|default:"—" }}<br>
                            {{ repo.last_imported|date:"Y-m-d H:i"|default:"—" }}
                        </td>
                        <td>
                            <strong>{{ repo.status|escape }}</strong>
                            {% if not repo.is_enabled %}<br><span class="text-muted">{_ Daily checks paused _}</span>{% endif %}
                            {% if repo.error %}<br><span class="text-danger">{_ See import report for error details _}</span>{% endif %}
                        </td>
                        <td>
                            {_ Imported _}: {{ repo.imported_count }}<br>
                            {_ Not imported _}: {{ repo.skipped_count }}<br>
                            {% button text=_"Import report" postback={report id=repo.id} delegate=`m_zotonicwww2_external` class="btn btn-default btn-xs" %}
                        </td>
                        <td>
                            {% button text=_"Edit" postback={edit id=repo.id} delegate=`m_zotonicwww2_external` class="btn btn-default btn-sm" %}
                            {% if repo.status != `deprecated` %}
                                {% button text=_"Deprecate" postback={deprecate id=repo.id} delegate=`m_zotonicwww2_external` class="btn btn-danger btn-sm" %}
                            {% endif %}
                            {% button text=_"Fetch and import now" postback={fetch id=repo.id} delegate=`m_zotonicwww2_external` class="btn btn-default btn-sm" %}
                        </td>
                    </tr>
                {% empty %}
                    <tr><td colspan="7">{_ No repositories have been added yet. _}</td></tr>
                {% endfor %}
            </tbody>
        </table>
    </div>
{% endif %}
{% endblock %}
