#!/usr/bin/env bash

jgdebug "Adding ruby to path"
PATH=~/.gem/ruby/2.7.0/bin:$PATH
PATH=/usr/local/opt/ruby/bin:$PATH
PATH="$(gem environment gemdir)/bin":$PATH
