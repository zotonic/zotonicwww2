---
name: "editor_survey_pages"
title: "Splitting a form into pages"
summary: "Group questions and show respondents only the sections they need."
category: "userguide"
language: "en"
is_published: true
parent: "editor_collection_surveys_forms"
order: 5
required_modules: ["mod_survey"]
zotonic_keywords: ["how_to_guide", "content_editor", "surveys", "forms"]
---

# Splitting a form into pages

Use several question pages when a form has distinct sections. Keep a short form on one page unless a split helps the respondent.

1. Open **Questions** and click **Add page**.
2. Add the questions for that section, with a heading or explanation where useful.
3. Save and use **View** to check the **Next**, **Back**, and final **Submit** steps.
4. In **Settings → Progress**, choose progress text or a progress bar if it helps people understand the length.

## Skip an irrelevant section

A page jump uses a question's name and its stored answer value. For example, give a single-choice question the name `visit_type`, with stored values `guided` and `independent`. Put guided-visit questions on the next page and a question named `contact_email` at the beginning of the following page.

At the end of the first page, click **Add page jump**. Enter `visit_type == "independent"` under **if**, and `contact_email` under **go to**. This skips the guided-visit page for independent visits. Use **Go to question** to check the destination, then save and test both choices.

Avoid jumps that send people around a loop or past questions everyone must answer. Removing a page also removes its questions. Do not select **Remove "Next" button** unless you have provided and tested another button that continues the form.

For complex conditions, ask your website manager to review the flow. Test every route, including going back and changing an earlier answer.
