# Use regular expressions with Erlang/OTP 28

OTP 28 uses PCRE2 for `re`. Re-test expressions carried over from older Erlang releases, especially Unicode classes and unusual escape sequences. See the [OTP 28 re reference](https://www.erlang.org/docs/28/apps/stdlib/re.html).

In a practice Erlang shell:

```erlang
re:run(<<"Order 123">>, <<"[0-9]+">>, [{capture, first, binary}]).
% {match,[<<"123">>]}
re:run(<<"Order abc">>, <<"[0-9]+">>, [{capture, none}]).
% nomatch
re:replace(<<"a   b">>, <<"\\s+">>, <<" ">>, [global, {return, binary}]).
% <<"a b">>
```

`+` repeats the preceding expression one or more times; `*` allows zero occurrences too. `^` and `$` anchor a match. Erlang string and binary literals need an extra backslash to pass a backslash to the regex engine.

For repeated matching, compile once and handle both success and failure:

```erlang
{ok, Pattern} = re:compile(<<"^[0-9]+$">>),
match = re:run(<<"123">>, Pattern, [{capture, none}]),
nomatch = re:run(<<"abc">>, Pattern, [{capture, none}]).
```

Treat the compiled pattern as opaque. Do not copy its printed internal tuple into source code. For UTF-8 input, use `unicode`; add `ucp` when character classes should use Unicode properties. Do not use a small regex as a complete email-address validator: use the form validator and verify address ownership when required.
