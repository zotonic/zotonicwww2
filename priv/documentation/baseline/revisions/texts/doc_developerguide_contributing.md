# Contribute a Zotonic change

Start with the repository's `README`, contributor instructions and current CI workflow. Use Erlang/OTP 28 for this documentation's development setup. Create a branch for one concrete change and keep unrelated formatting out of it.

1. Reproduce the issue or describe the missing behaviour.
2. Make the smallest maintainable change, following the surrounding Erlang and template conventions.
3. Run the build and checks required by the checkout. Use a disposable test site for tests that change resources or schemas.
4. Add a focused regression test when it verifies a meaningful behaviour.
5. Update affected documentation and describe the observable result and validation in the pull request.

Current reference documentation is maintained with the relevant source; do not assume the former Sphinx import layout still owns every page. For module documentation, keep examples and `zotonic_keywords` aligned with the implemented API. Avoid promises about release dates or lint tools that are not part of the current repository workflow.

Report security-sensitive issues through the repository's security reporting policy rather than including an exploit or credentials in a public issue.
