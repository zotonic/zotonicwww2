---
name: "editor_survey_choices"
title: "Adding answer choices"
summary: "Offer clear choices while preserving the values used in stored answers."
category: "userguide"
language: "en"
is_published: true
parent: "editor_collection_surveys_forms"
order: 4
required_modules: ["mod_survey"]
zotonic_keywords: ["how_to_guide", "content_editor", "surveys", "forms"]
---

# Adding answer choices

Use **Multiple choice or quiz question (Thurstone)** for a question such as “Which session would you like to attend?”

1. Add the question and enter its name and prompt.
2. Choose **Single answer possible**, **Drop-down menu**, or **Multiple answers possible**.
3. Enter an **Answer** label for the first choice.
4. Use **Add answer** for each extra choice.
5. Give each choice a unique **Stored value**, such as `morning` or `afternoon`. Keep these values stable if you translate or reword the labels.
6. Choose whether an answer is required. Save and test every choice.

For a registration, offer “Morning visit” and “Afternoon visit”. Do not enable **Quiz or test question** unless there are right and wrong answers.

Use multiple answers only when selecting several options makes sense. Add “Other” or “Not applicable” when a forced choice would produce misleading responses.

**Submit on clicking an option** advances the form as soon as a choice is made. Use it only when respondents need no other input on that question page, and test the resulting flow. **Randomize answers** changes the display order; avoid it for ordered scales or choices that depend on their position.

Do not reuse an existing stored value for a different meaning after collecting responses. Create a new version of the form when the meaning of the question changes.
