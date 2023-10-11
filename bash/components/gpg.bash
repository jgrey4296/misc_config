#!/usr/bin/env bash

GPG_TTY=$(tty)
GNUPGHOME="$BASE_CONFIG/gnupg"

# Example password retrieval from https://gist.github.com/vincentbernat/532ce51f278146a30e11
# "echo ${PASSWORD:-$(gpg --no-tty -qd ~/.authinfo.gpg | sed -n 's,^machine imap.luffy.cx .*password \\([^ ]*\\).*,\\1,p')}"
