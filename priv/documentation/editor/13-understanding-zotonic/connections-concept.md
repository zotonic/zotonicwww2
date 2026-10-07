---
name: "editor_connections_concept"
title: "What connections mean"
summary: "Understand how separate content works together."
category: "userguide"
language: "en"
is_published: true
parent: "editor_collection_understanding_zotonic"
order: 3
required_modules: []
zotonic_keywords: ["explanation", "content_editor", "edge", "predicate", "content_relationships"]
---

# What connections mean

::: aside
Developers call connections **edges** and connection types **predicates**. See `model#edge` for the technical reference.
:::

A connection records a named relationship from one resource to another.

For example, an article can point to a person as its **Author**, a keyword as its **Subject**, and an image as its **Depiction**. The admin may show friendlier labels, such as **Keyword** or **Attached media**.

Connections have a direction. The article is connected to its author; the person's page can show that the article is connected from it.

A connection is different from a link typed into a paragraph. It gives the website structured information that can be used in layouts, lists, and related content.

