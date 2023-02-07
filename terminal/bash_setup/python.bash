#!/usr/bin/env bash

jgd Python Aliases

CONDA_DEFAULT_ENV=base310

alias pServer="python -m http.server 8888 &"
alias qpServer="(python -m http.server 8888 > /dev/null 2>&1) & && (echo 'Running Quiet Server')"
alias qpoServer="((python -m http.server 8888 > /dev/null 2>&1) &) && (echo 'Running Quiet Server') && (open http://localhost:8888/)"

#for automating the update of pip packages:
alias pipupdate="pip list --outdated --format=freeze | grep -v '^\-e' | cut -d = -f 1 | xargs -n1 pip install -U"

# conda environments
alias cenv="conda activate"
alias clist="conda env list"
alias cexport="conda env export --from-history"

#Caffe Stuff:
#DYLD_FALLBACK_LIBRARY_PATH=/usr/local/cuda/lib:$HOME/.pyenv/versions/anaconda-2.2.0/lib:/usr/local/lib/:/usr/lib

#NLTK:
NLTK_DATA=~/assets/nlg/nltk
