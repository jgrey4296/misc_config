#!/usr/bin/env bash

jgdebug "Python setup"

PYTHONSTARTUP="$HOME/.config/jg/templates/python/repl_startup.py"
IPYTHONDIR="$HOME/.config/jg/templates/python/"

#Caffe Stuff:
#DYLD_FALLBACK_LIBRARY_PATH=/usr/local/cuda/lib:$HOME/.pyenv/versions/anaconda-2.2.0/lib:/usr/local/lib/:/usr/lib
DYLD_FALLBACK_LIBRARY_PATH="${JGCACHE}/dylibs/:${DYLD_FALLBACK_LIBRARY_PATH}"

#NLTK:
NLTK_DATA="${JGCACHE}/assets/nlg/nltk"

MANPATH="${ANACONDA_HOME}/man:$MANPATH"
