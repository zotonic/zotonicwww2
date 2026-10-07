# Understand pages and connections

Almost everything you edit in Zotonic is a resource: an article, person, image, keyword or collection. Each resource has an ID, properties such as a title, and one category. A unique name gives developers and imports a stable way to find it even when its title changes.

A category describes what the resource is. Categories form a hierarchy: an image is also media. Categories affect the available fields and how a page is displayed.

A connection joins two resources in a direction. Its predicate describes the relationship. For example, an article can have an outgoing `author` connection to a person; a collection has outgoing `haspart` connections to its members. The order of those members is part of the collection's structure.

Use existing keywords through `subject` connections to make content discoverable. Add a relationship instead of copying the same information into several bodies. For these documentation guides, related tasks use `relation`, and reference material uses `hasreference`. Automatically discovered body references use `refers` and are maintained by admin.

Publishing and access are separate. A page may be marked published but be outside its publication period or restricted to a user group. Test it as the intended reader. Being allowed to edit a page does not imply permission to link every other resource.
