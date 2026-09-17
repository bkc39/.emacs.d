#!/bin/sh
# Fail when a tracked file contains an absolute home-directory path.
# Such paths name a person and a machine, and they belong in the
# untracked local.eld (see local.example.eld).
set -eu
cd "$(git rev-parse --show-toplevel)"

pattern='(/home|/Users)/[A-Za-z0-9._-]+/'

if git grep -nIE "$pattern" -- .; then
    echo >&2
    echo "check-tree: absolute home-directory paths found in tracked files." >&2
    echo "check-tree: move them into local.eld and read them with local-config-get." >&2
    exit 1
fi
echo "check-tree: ok"
