{% if m.rsc[module_id].doc_module_config as configs %}
    <section class="connections" id="module-configuration">
        <h3>{_ Configuration _}</h3>
        <p>{_ Configuration keys declared by this module. The defaults below are from the source code. _}</p>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th scope="col">{_ Module _}</th>
                        <th scope="col">{_ Key _}</th>
                        <th scope="col">{_ Type _}</th>
                        <th scope="col">{_ Default _}</th>
                        <th scope="col">{_ Description _}</th>
                    </tr>
                </thead>
                <tbody>
                    {% for config in configs %}
                        <tr>
                            <td><code>{{ config.module }}</code></td>
                            <th scope="row"><code>{{ config.key }}</code></th>
                            <td><code>{{ config.type|default:"—" }}</code></td>
                            <td>{% if config.has_default %}<code>{{ config.default }}</code>{% else %}—{% endif %}</td>
                            <td>{{ config.description }}</td>
                        </tr>
                    {% endfor %}
                </tbody>
            </table>
        </div>
    </section>
{% endif %}
