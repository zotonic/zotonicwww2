---
name: "editor_survey_email"
title: "Sending form notifications and confirmations"
summary: "Notify the responsible team and tell respondents their form was received."
category: "userguide"
language: "en"
is_published: true
parent: "editor_collection_surveys_forms"
order: 7
required_modules: ["mod_survey"]
zotonic_keywords: ["how_to_guide", "content_editor", "surveys", "forms", "email_delivery"]
---

# Sending form notifications and confirmations

Open **Settings → Handling** on the survey.

1. In **Mail results to**, enter the addresses of the people responsible for following up. Separate addresses with commas.
2. Enable **Send a confirmation email to the respondent** if a receipt is useful.
3. For anonymous visitors, add a short-answer question with the exact name `email` and validation **must be an email address**. The visible prompt can be “Your email address”. For logged-in respondents, the system can use their known account email address.
4. Choose which answers the confirmation includes: all, closed questions only, or no answers. There are separate choices for anonymous and logged-in respondents.
5. Write the **Introduction for confirmation email** and save.
6. Submit a clearly labelled test using an address you control. Check the staff notification and respondent receipt, including their language and answer content.

A thank-you screen confirms the form flow, not delivery to an inbox. If an email is missing, check spam and ask your website manager to inspect delivery. The site must have working outbound email.

A confirmation is not a newsletter subscription. Collect and use addresses for the purpose explained on the form. Avoid including unnecessary personal details in emails, and translate the confirmation text when the form has several languages.

A special form handler may change the normal storage or email behaviour. Confirm that arrangement before relying on either notifications or the Results tab.
