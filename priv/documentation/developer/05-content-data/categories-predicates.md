---
name: "developer_categories_predicates"
title: "Model content with categories and predicates"
summary: "Use a category to describe what a resource is, such as an article or event. Use a predicate to describe a relationship between two resources, such as an author or subject."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_content_data"
order: 4
required_modules: []
source_paths: ["apps/zotonic_core/src/models", "apps/zotonic_core/src/support/z_datamodel.erl", "apps/zotonic_core/src/support/z_search.erl"]
zotonic_keywords: ["explanation", "backend_developer", "category", "predicate", "content_modeling"]
---

# Model content with categories and predicates

Use a category to describe what a resource is, such as an article or event. Use a predicate to describe a relationship between two resources, such as an author or subject.

Start from existing categories and predicates before adding new ones. Category inheritance affects template selection and permissions, so test the effect of moving content into a new category.

Define the allowed subject and object categories for a predicate to help editors make useful connections. Use the relationship in searches and templates rather than duplicating the connected resource's title in another field.
