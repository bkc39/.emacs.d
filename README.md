# .emacs.d
Landscape for my zen garden.

Depends on `use-package` and `straight.el`. The automatic install
requires an internet connection. Your first time starting emacs will
take a while because it will install everything that first go. After
that it'll be buttery, though.

I use this on MacOS and Ubuntu mostly.

## Local configuration

Anything that belongs to one machine, one employer or one person stays
out of this repository. It lives in an untracked file, `local.eld`, next
to `init.el`.

- Copy `local.example.eld` to `local.eld` and edit it. Every key is
  optional, and a missing key falls back to a neutral default.
- The file is a single s-expression. It is parsed with `read` and never
  evaluated, so it is data and cannot run code.
- Read a value with `(local-config-get '(python interpreter) "python3")`.
  See `elisp/local-config.el`.

Enable the commit guard once per clone:

    git config core.hooksPath hooks

The guard rejects absolute home-directory paths, and it rejects staged
lines that match the `deny-patterns` regexps in your `local.eld`. The
patterns stay in the untracked file, so the denylist is never published.
CI runs the path check and the tests in `test/`.
