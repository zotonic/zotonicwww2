# Normalize and filter Unicode text

Erlang binaries contain bytes; a UTF-8 character can span several bytes. Do not filter a binary byte by byte to remove “non-ASCII” values: that destroys valid text.

Normalize valid Unicode input and use character-aware operations:

```erlang
Text = unicode:characters_to_nfc_binary("Café"),
Clean = re:replace(Text, <<"[\\r\\n\\t]+">>, <<" ">>,
                   [unicode, global, {return, binary}]).
```

The result retains `é` and replaces line breaks and tabs with spaces. Check `unicode:characters_to_binary/1` results at an input boundary: invalid or incomplete encoded input can return an error tuple. Decide whether to reject or repair it instead of silently dropping bytes.

Choose escaping for the output context. HTML uses HTML escaping, JSON uses a JSON encoder, and a LaTeX document needs a deliberate TeX-text encoder for characters such as backslash, braces, percent and ampersand. A list of character substitutions is not a general-purpose sanitizer for all three formats.
