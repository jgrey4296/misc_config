#!/usr/bin/env bash

jgdebug "Adding ruby to path"

# PATH="$(gem environment user_gemhome)/bin":$PATH
PATH="$(gem environment gemdir)/bin":$PATH
