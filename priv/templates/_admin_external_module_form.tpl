{% if m.acl.is_admin %}
{% wire id=#form type="submit" postback={save id=repo.id} delegate=`m_zotonicwww2_external` %}
<form id="{{ #form }}" method="post" action="postback">
    <div class="form-group">
        <label for="{{ #title }}">{_ Name _}</label>
        <input class="form-control" id="{{ #title }}" name="title" value="{{ repo.title|escape }}" required maxlength="200">
    </div>
    <div class="form-group">
        <label for="{{ #git }}">{_ Git URL _}</label>
        <input class="form-control" id="{{ #git }}" name="git_url" type="url" value="{{ repo.git_url|escape }}" placeholder="https://github.com/owner/repository.git" required>
    </div>
    <div class="form-group">
        <label for="{{ #website }}">{_ Information URL (if different) _}</label>
        <input class="form-control" id="{{ #website }}" name="website_url" type="url" value="{{ repo.website_url|escape }}">
    </div>
    <div class="form-group">
        <label for="{{ #branch }}">{_ Branch (leave empty for the default branch) _}</label>
        <input class="form-control" id="{{ #branch }}" name="branch" value="{{ repo.branch|escape }}">
    </div>
    <div class="form-group">
        <label for="{{ #hex }}">{_ Hex package _}</label>
        <input class="form-control" id="{{ #hex }}" name="hex_package" value="{{ repo.hex_package|escape }}">
    </div>
    <div class="checkbox"><label><input type="checkbox" name="is_enabled" value="1" {% if not repo or repo.is_enabled %}checked{% endif %}> {_ Check for updates daily _}</label></div>
    <div class="modal-footer">
        {% button text=_"Cancel" action={dialog_close} class="btn btn-default" %}
        <button class="btn btn-primary" type="submit">{_ Save and queue import _}</button>
    </div>
</form>
{% endif %}
