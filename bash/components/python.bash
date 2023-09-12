#!/usr/bin/env bash

jgdebug "Python setup"

PYTHONSTARTUP="$HOME/.doom.d/templates/python/repl_startup.py"
IPYTHONDIR="$HOME/.doom.d/templates/python/"

#Caffe Stuff:
#DYLD_FALLBACK_LIBRARY_PATH=/usr/local/cuda/lib:$HOME/.pyenv/versions/anaconda-2.2.0/lib:/usr/local/lib/:/usr/lib
DYLD_FALLBACK_LIBRARY_PATH="${HOME}/.emacs.d/.local/modules/:${DYLD_FALLBACK_LIBRARY_PATH}"

#NLTK:
NLTK_DATA="${HOME}/assets/nlg/nltk"

MANPATH="${ANACONDA_HOME}/man:$MANPATH"
