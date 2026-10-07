# Add an icon to a control

An icon class works only when its font or stylesheet is loaded. The standard admin uses Bootstrap-era icon classes; a custom frontend may use a different set. Inspect the active theme rather than assuming every site loads the same icon library.

For a button in admin:

```django
<button type="button" class="btn btn-default">
    <span class="glyphicon glyphicon-search" aria-hidden="true"></span>
    {_ Search _}
</button>
```

Keep visible text where possible. An icon-only control needs an accessible name such as `aria-label`, and decorative icons should be hidden from assistive technology. Use `tag#lib` to include any additional icon assets through the site's normal asset pipeline.

Test the control with the font loaded, with keyboard navigation and at increased zoom. Do not add an old LESS build chain just to copy an icon example.
