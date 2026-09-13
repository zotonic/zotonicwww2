{#
    Medium records and EXIF data are not sanitized. Escape every rendered
    field at this output boundary, including values formatted by filters.
#}
<section class="media-metadata" aria-labelledby="media-metadata-title">
    <header class="media-metadata__header">
        <p>{_ Media _}</p>
        <h2 id="media-metadata-title">{_ File information _}</h2>
    </header>

    <dl class="media-metadata__facts">
        {% if medium.mime %}
            <div>
                <dt>{_ Format _}</dt>
                <dd>{{ medium.mime|escape }}</dd>
            </div>
        {% endif %}
        {% if medium.width and medium.height %}
            <div>
                <dt>{_ Dimensions _}</dt>
                <dd>{{ medium.width|escape }} × {{ medium.height|escape }} {_ pixels _}</dd>
            </div>
        {% endif %}
        {% if medium.duration %}
            <div>
                <dt>{_ Duration _}</dt>
                <dd>{{ medium.duration|format_duration|escape }}</dd>
            </div>
        {% endif %}
        {% if medium.size %}
            <div>
                <dt>{_ File size _}</dt>
                <dd>{{ medium.size|filesizeformat|escape }}</dd>
            </div>
        {% endif %}
        {% if medium.original_filename %}
            <div>
                <dt>{_ Original filename _}</dt>
                <dd>{{ medium.original_filename|escape }}</dd>
            </div>
        {% endif %}
    </dl>

    {% if id.is_a.image and medium.exif %}
        {% with medium.exif as exif %}
            {% if exif.make or exif.model or exif.date_time_original or exif.exposure_time or exif.f_number or exif.focal_length or exif.iso_speed_ratings %}
                <div class="media-metadata__camera">
                    <h3>{_ Photograph details _}</h3>
                    <dl class="media-metadata__facts media-metadata__facts--camera">
                        {% if exif.make or exif.model %}
                            <div>
                                <dt>{_ Camera _}</dt>
                                <dd>
                                    {% if exif.make %}{{ exif.make|media_exif_value:"make"|escape }}{% endif %}
                                    {% if exif.make and exif.model %} · {% endif %}
                                    {% if exif.model %}{{ exif.model|media_exif_value:"model"|escape }}{% endif %}
                                </dd>
                            </div>
                        {% endif %}
                        {% if exif.date_time_original %}
                            <div>
                                <dt>{_ Captured _}</dt>
                                <dd>{{ exif.date_time_original|media_exif_value:"date_time_original"|escape }}</dd>
                            </div>
                        {% endif %}
                        {% if exif.exposure_time %}
                            <div>
                                <dt>{_ Exposure _}</dt>
                                <dd>{{ exif.exposure_time|media_exif_value:"exposure_time"|escape }}</dd>
                            </div>
                        {% endif %}
                        {% if exif.f_number %}
                            <div>
                                <dt>{_ Aperture _}</dt>
                                <dd>{{ exif.f_number|media_exif_value:"f_number"|escape }}</dd>
                            </div>
                        {% endif %}
                        {% if exif.focal_length %}
                            <div>
                                <dt>{_ Focal length _}</dt>
                                <dd>{{ exif.focal_length|media_exif_value:"focal_length"|escape }}</dd>
                            </div>
                        {% endif %}
                        {% if exif.iso_speed_ratings %}
                            <div>
                                <dt>{_ ISO _}</dt>
                                <dd>{{ exif.iso_speed_ratings|media_exif_value:"iso_speed_ratings"|escape }}</dd>
                            </div>
                        {% endif %}
                    </dl>
                </div>
            {% endif %}

            <details class="media-metadata__exif">
                <summary>
                    <span>{_ All EXIF fields _}</span>
                    <small>{{ exif|length|escape }} {_ fields _}</small>
                </summary>
                <dl class="media-metadata__exif-list">
                    {% for name, value in exif|sort %}
                        <div>
                            <dt>{{ name|to_binary|replace:"_":" "|capfirst|escape }}</dt>
                            <dd>{{ value|media_exif_value:name|escape }}</dd>
                        </div>
                    {% endfor %}
                </dl>
            </details>
        {% endwith %}
    {% endif %}
</section>
