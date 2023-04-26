#!/usr/bin/env bash

jgdebug "Python Aliases"

#Caffe Stuff:
#DYLD_FALLBACK_LIBRARY_PATH=/usr/local/cuda/lib:$HOME/.pyenv/versions/anaconda-2.2.0/lib:/usr/local/lib/:/usr/lib
DYLD_FALLBACK_LIBRARY_PATH="$HOME/.emacs.d/.local/modules/":$DYLD_FALLBACK_LIBRARY_PATH

#NLTK:
NLTK_DATA=~/assets/nlg/nltk

MANPATH=/usr/local/anaconda3/man:$MANPATH
