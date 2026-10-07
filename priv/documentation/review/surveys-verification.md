# Surveys and forms — 7 October 2026

Added one editor collection and 12 task pages. Coverage includes first-time form
creation, question names and validation, choices, page jumps, answering rules,
notifications, testing and publication, reviewing responses, exports, closing a
form, and quizzes. The integration map and page inventory include every addition.
Navigation uses ordered `haspart`, `relation`, and `hasreference` connections.

## Local browser checks

Used the English standard admin at `zotonicwww2.test:8443`, authenticated as admin.
`mod_survey` and `mod_export` were already enabled.

- Created unpublished Survey `editor_guide_example_survey` (local ID 14601),
  titled **Community garden visit**, with name, validated email, and yes/no fields.
- Opened the respondent preview and confirmed an invalid email was rejected.
- Submitted **Documentation test**, `editor-guide@example.org`, and Yes.
  The thank-you screen appeared and the Results editor showed the saved response
  (local answer ID 39, associated with the authenticated administrator).
- Opened **View answers** and checked respondent answers, internal status/note,
  **Edit all answers**, **Save**, and **Save & Email** controls.
- Captured the Questions tab, Settings tab, and Download dialog as three JPEGs.
  None contains the site-specific documentation dashboard.

The example remains unpublished for future screenshot work. No survey email was
sent and no response was deleted. It is local test data, not part of the guide's
production import.

## Source checks

Checked the current `zotonic_mod_survey` module, question implementations, model,
and admin templates for field names, repeat submissions, saved progress, editor-only
answers, status notes, confirmation email selection, export options, uploads,
deactivation, and quiz scoring. The documented upload behaviour follows the
standard handler's attachment code; uploads are not normal stored answers.

Evaluated the documented page-jump condition through `survey_q_page_break:test/3`:
`visit_type == "independent"` jumps to `contact_email` for the independent value
(binary or single-item list) and does not jump for `guided`.

The existing reference resource `doc_module_mod_survey` was resolved locally as
12770 and registered as a `hasreference` target.

## Limits and publication checks

The browser check confirms the simple authenticated form flow. It does not prove
anonymous access, confirmation delivery, downloaded file contents, saved-progress
behaviour, file delivery, or all quiz and branching combinations. The relevant
tasks explicitly instruct editors to test their actual configuration. Review
those paths on the destination before publishing a form that depends on them.

Render both documentation bundles and run the connection checks after edits.
The screenshots and bodies remain staging sources; production import still needs
media upload, target-name resolution, and destination checks.
