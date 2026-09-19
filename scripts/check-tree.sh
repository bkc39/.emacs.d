#!/bin/sh
# Fail when a tracked file contains an absolute home-directory path or
# an email address.  Both identify a person or a machine, and they
# belong in the untracked local.eld (see local.example.eld).
set -eu
cd "$(git rev-parse --show-toplevel)"
status=0

pattern='(/home|/Users)/[A-Za-z0-9._-]+/'

if git grep -nIE "$pattern" -- .; then
    echo >&2
    echo "check-tree: absolute home-directory paths found in tracked files." >&2
    echo "check-tree: move them into local.eld and read them with local-config-get." >&2
    status=1
fi

# Placeholder domains, GitHub noreply addresses and git-over-ssh remotes
# are not personal addresses.
email='[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}'
allowed='@(example\.(com|org|net|invalid)|users\.noreply\.github\.com)|(^|[^A-Za-z0-9._%+-])git@'

if git grep -nIE "$email" -- . | grep -vE "$allowed"; then
    echo >&2
    echo "check-tree: email addresses found in tracked files." >&2
    echo "check-tree: put identity under the identity key of local.eld." >&2
    status=1
fi

[ "$status" -eq 0 ] && echo "check-tree: ok"
exit "$status"
