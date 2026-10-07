# Keyword audit — 7 October 2026

Reviewed the 318 authored assignments and 94 baseline assignments, including the final 383 destination resources after adoption. Checked controlled vocabulary, information type, intended audience, subject relevance and missing central topics. Changes below are based on current replacement texts, not the immutable captured exports.

Corrected 45 source assignments, affecting 45 destination pages. The combined plan contains 1575 subject connections. All assignments have known, unique keywords and the required type, audience and subject facets.

## Findings

- Use content_relationships for editorial collections; collection is a programming datatype.
- Classify routine tasks, explanations, troubleshooting and command reference according to the text.
- Include security for template escaping, surveys for contact forms, and schedule for scheduled mail and background tasks.
- Update baseline keywords for rewritten articles: outbound email is an operator task, and Erlang signup observers target backend developers.
- Preserve concise topic sets: linked articles and incidental technologies do not automatically become keywords.

## Corrections

| Source name | Previous keywords | Reviewed keywords | Reason |
| --- | --- | --- | --- |
| editor_preview | how_to_guide, content_editor, publishing, validate | how_to_guide, content_editor, publishing, editorial_workflow | Previewing is an editorial check, not input validation. |
| editor_collections | how_to_guide, content_editor, navigation, collection | how_to_guide, content_editor, navigation, content_relationships | Content collections use Has part connections; collection is the taxonomy datatype facet. |
| editor_content_order | how_to_guide, content_editor, navigation, collection | how_to_guide, content_editor, navigation, content_relationships | Content collections use Has part connections; collection is the taxonomy datatype facet. |
| editor_language_urls | how_to_guide, content_editor, localization_and_translation, translated_text | how_to_guide, content_editor, localization_and_translation, url | The task concerns translated URLs. |
| editor_schedule_mailing | how_to_guide, content_editor, mailing_lists, email_delivery | how_to_guide, content_editor, mailing_lists, email_delivery, schedule | Scheduling is the central task. |
| editor_menus_collections | explanation, content_editor, navigation, collection | explanation, content_editor, navigation, content_relationships | Content collections use Has part connections; collection is the taxonomy datatype facet. |
| editor_survey_quiz | how_to_guide, content_editor, surveys, forms, validate | how_to_guide, content_editor, surveys, forms | Quiz scoring is not the validation task facet. |
| developer_runtime_storage | how_to_guide, backend_developer, file_storage, file_store | explanation, backend_developer, file_storage, file_store | Explains storage boundaries rather than a procedural task. |
| developer_template_values | how_to_guide, frontend_developer, template, data_processing_and_formatting | how_to_guide, frontend_developer, template, security, html | Escaping untrusted HTML is the central security concern. |
| developer_resource_properties | explanation, backend_developer, resource, structured_data | how_to_guide, backend_developer, resource, structured_data | Provides a worked read/update procedure. |
| developer_browser_debugging | how_to_guide, frontend_developer, development_and_debugging, javascript, cotonic | troubleshooting, frontend_developer, development_and_debugging, javascript, cotonic | Diagnoses a failing browser interaction. |
| developer_cache_debugging | how_to_guide, backend_developer, development_and_debugging, cache, performance | troubleshooting, backend_developer, development_and_debugging, cache, performance | Diagnoses stale content or code. |
| developer_collection_command_reference | explanation, backend_developer, development_and_debugging, erlang_otp | reference, backend_developer, development_and_debugging, erlang_otp | Entry point for command syntax lookup. |
| admin_upgrade | how_to_guide, operator, site_management, reliability | how_to_guide, operator, site_management, reliability, migrate | Explicitly covers upgrade and schema migration planning. |
| admin_health_checks | troubleshooting, operator, logging_and_monitoring, reliability, monitor | how_to_guide, operator, logging_and_monitoring, reliability, monitor | Routine monitoring rather than incident diagnosis. |
| doc_developerguide_email | explanation, backend_developer, email, email_delivery, email_receiving | how_to_guide, operator, email, email_delivery, configuration | Align with the reviewed replacement body and its intended reader. |
| doc_developerguide_resources | explanation, backend_developer, resource, content_modeling | how_to_guide, backend_developer, resource, content_modeling, content_relationships | Align with the reviewed replacement body and its intended reader. |
| doc_developerguide_media | explanation, backend_developer, media_management, media_resource | how_to_guide, frontend_developer, backend_developer, media_management, media_resource, template | Align with the reviewed replacement body and its intended reader. |
| doc_developerguide_server_browser_interaction | explanation, backend_developer, messaging_and_pubsub, mqtt, cotonic | how_to_guide, frontend_developer, backend_developer, wire_action, messaging_and_pubsub, api_and_integration | Align with the reviewed replacement body and its intended reader. |
| doc_developerguide_templates | explanation, frontend_developer, template, render | how_to_guide, frontend_developer, template, render, security, cache | Align with the reviewed replacement body and its intended reader. |
| doc_developerguide_modules | explanation, backend_developer, module, module_management | how_to_guide, backend_developer, module, module_management, migrate | Align with the reviewed replacement body and its intended reader. |
| doc_developerguide_sites | explanation, backend_developer, site, site_management | tutorial, backend_developer, site, site_management, configuration | Align with the reviewed replacement body and its intended reader. |
| doc_bestpractices_creating_sites | explanation, backend_developer, site_management, maintainability | tutorial, backend_developer, site_management, content_modeling, resource | Align with the reviewed replacement body and its intended reader. |
| doc_developerguide_icons | explanation, frontend_developer, template, user_interface_and_interaction | how_to_guide, frontend_developer, template, development_and_debugging | Align with the reviewed replacement body and its intended reader. |
| doc_bestpractices_media | explanation, backend_developer, media_management, media_resource | how_to_guide, frontend_developer, backend_developer, media_management, media_resource, template, image_management | Align with the reviewed replacement body and its intended reader. |
| doc_bestpractices_templates | explanation, frontend_developer, template, render | how_to_guide, frontend_developer, template, render, security | Align with the reviewed replacement body and its intended reader. |
| doc_developerguide_css_classes | explanation, frontend_developer, template, user_interface_and_interaction | reference, frontend_developer, template, user_interface_and_interaction | Align with the reviewed replacement body and its intended reader. |
| doc_cookbook_custom_filter | cookbook_recipe, backend_developer, template_filter, data_processing_and_formatting | cookbook_recipe, backend_developer, template_filter, scomp, template | Align with the reviewed replacement body and its intended reader. |
| doc_cookbook_frontend_contactform | cookbook_recipe, frontend_developer, forms, email, template | cookbook_recipe, frontend_developer, backend_developer, forms, surveys, email_delivery, security | Align with the reviewed replacement body and its intended reader. |
| doc_cookbook_frontend_sitespecific_signup | cookbook_recipe, frontend_developer, identity_and_accounts, notification | cookbook_recipe, backend_developer, identity_and_accounts, notification | Align with the reviewed replacement body and its intended reader. |
| doc_cookbook_frontend_signup_redirection | cookbook_recipe, frontend_developer, authentication, routing_and_redirects | cookbook_recipe, backend_developer, authentication, routing_and_redirects | Align with the reviewed replacement body and its intended reader. |
| doc_cookbook_custom_action | cookbook_recipe, backend_developer, wire_action, user_interface_and_interaction | cookbook_recipe, backend_developer, frontend_developer, wire_action, user_interface_and_interaction | Align with the reviewed replacement body and its intended reader. |
| doc_cookbook_shell_activate_modules | troubleshooting, backend_developer, module_management, erlang_otp | cookbook_recipe, backend_developer, module_management, erlang_otp | Align with the reviewed replacement body and its intended reader. |
| doc_cookbook_task_queue | cookbook_recipe, backend_developer, scheduled_and_background_work, background_task | cookbook_recipe, backend_developer, scheduled_and_background_work, background_task, schedule | Align with the reviewed replacement body and its intended reader. |
| doc_developerguide_deployment_server_configuration | explanation, operator, configuration, reliability | how_to_guide, operator, configuration, reliability | The replacement gives instructions for completing a task. |
| doc_developerguide_deployment_env | explanation, operator, configuration, site_management | how_to_guide, operator, configuration, site_management | The replacement gives instructions for completing a task. |
| doc_developerguide_dispatch_rules | explanation, backend_developer, dispatch_rule, routing_and_redirects | how_to_guide, backend_developer, dispatch_rule, routing_and_redirects | The replacement gives instructions for completing a task. |
| doc_developerguide_contributing | explanation, backend_developer, development_and_debugging, maintainability | how_to_guide, backend_developer, development_and_debugging, maintainability | The replacement gives instructions for completing a task. |
| doc_developerguide_deployment_https | explanation, operator, tls_and_certificates, security, configuration | how_to_guide, operator, tls_and_certificates, security, configuration | The replacement gives instructions for completing a task. |
| doc_developerguide_deployment_privilegedports | explanation, operator, configuration, http, security | how_to_guide, operator, configuration, http, security | The replacement gives instructions for completing a task. |
| doc_developerguide_deployment_nginx | explanation, operator, http, configuration, tls_and_certificates | how_to_guide, operator, http, configuration, tls_and_certificates | The replacement gives instructions for completing a task. |
| doc_developerguide_deployment_startup | explanation, operator, site_management, configuration, reliability | how_to_guide, operator, site_management, configuration, reliability | The replacement gives instructions for completing a task. |
| doc_developerguide_deployment_varnish | explanation, operator, cache, performance, http | how_to_guide, operator, cache, performance, http | The replacement gives instructions for completing a task. |
| doc_userguide_issues_and_features | explanation, content_editor, development_and_debugging | how_to_guide, content_editor, development_and_debugging | The replacement gives instructions for completing a task. |
| doc_developerguide_download | explanation, backend_developer, site_management | how_to_guide, backend_developer, site_management | The replacement gives instructions for completing a task. |

## Scope and import behavior

The audit covers the captured documentation inventory, not every live production article. Source snapshots and their hashes remain unchanged. The portable plan is rebuilt from these assignments. Import reconciles only previously importer-owned subject edges; existing unmanaged keywords are preserved. This audit does not change production data.
## Verification

- Rebuilt `import/plan.json` successfully: 383 pages, 28 media, 793 connection groups.
- All 22 documentation tests pass, including source checksum verification.
- Compared with the committed plan: resource texts, media and non-subject connections are unchanged. Only subject connections, required keyword names and source checksums changed.
- All 383 final assignments pass taxonomy and facet validation.
- No test-site or production import was run as part of this audit; the corrected assignments take effect on the next import.
