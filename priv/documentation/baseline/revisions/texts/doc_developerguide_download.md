# Get Zotonic

Use [Zotonic releases](https://github.com/zotonic/zotonic/releases) for a tagged release. Read its requirements and upgrade notes before using it with an existing database.

For a development checkout:

```sh
git clone https://github.com/zotonic/zotonic.git
cd zotonic
```

Then follow the local-environment guide: choose containers, a native installation, or Nix; prepare dependencies; build; start; and create a practice site. This documentation advises **Erlang/OTP 28**. Use a supported PostgreSQL major release that meets Zotonic's requirements, with its current maintenance updates.

Do not treat the old `1.0.0-rc.7` download link as the current release, or use the unauthenticated `git://` clone URL from older instructions.
