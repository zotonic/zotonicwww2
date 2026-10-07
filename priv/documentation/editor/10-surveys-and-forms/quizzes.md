---
name: "editor_survey_quiz"
title: "Creating a quiz"
summary: "Mark correct answers, set scores, and choose the feedback respondents see."
category: "userguide"
language: "en"
is_published: true
parent: "editor_collection_surveys_forms"
order: 12
required_modules: ["mod_survey"]
zotonic_keywords: ["how_to_guide", "content_editor", "surveys", "forms", "validate"]
---

# Creating a quiz

Start with **Multiple choice or quiz question (Thurstone)** when you need scored answers.

1. Add a question, its answer choices, and their unique stored values.
2. Check **Quiz or test question**.
3. Mark the correct choices under **Correct** and set their **Points**.
4. Choose whether only one answer or multiple answers are possible.
5. Add correct/incorrect feedback where useful. Enable **Instant feedback (Learning Mode)** only when people should see feedback while answering.
6. In the survey's **Settings**, set **Test pass percentage** and choose the result shown after submission.
7. Save and test an all-correct response, an incorrect response, and a response near the pass threshold. Check the stored points and the final feedback.

Multiple-answer scoring can award points both for selecting correct choices and for not selecting wrong choices. **Subtract points for wrong answers** also penalizes omissions of correct choices; an individual question's score does not go below zero. Check the scoring with examples rather than assuming it is one point per question.

Matching questions can provide correctness feedback, but currently do not add points to the stored total. Do not mix them into a scored quiz without accounting for that distinction.

If you show results only after a pass, test both passing and failing outcomes so respondents receive a useful completion message in either case.
