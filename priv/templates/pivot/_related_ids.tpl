{#
    Restrict related-resource matching to subject keywords. The default Zotonic
    pivot includes object ids from every predicate, the content group, and the
    category hierarchy. `match_objects` searches these zpo tokens without
    retaining their predicate, so indexing only subjects keeps both sides of
    the related-documentation match predicate-pure.
#}
{% for subject_id in id.o.subject %}
    zpo{{ subject_id }}
{% endfor %}
