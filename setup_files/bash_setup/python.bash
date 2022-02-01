#!/usr/bin/env bash

alias pServer="python -m http.server 8888 &"
alias qpServer="(python -m http.server 8888 > /dev/null 2>&1) & && (echo 'Running Quiet Server')"
alias qpoServer="((python -m http.server 8888 > /dev/null 2>&1) &) && (echo 'Running Quiet Server') && (open http://localhost:8888/)"

#for automating the update of pip packages:
alias pipupdate="pip list --outdated --format=freeze | grep -v '^\-e' | cut -d = -f 1 | xargs -n1 pip install -U"

# conda environments
alias cenv="conda activate"
alias clist="conda env list"
alias cexport="conda env export --from-history"


function set_non_standard_python_paths(){
    echo "Setting Non-Standard Python Paths"
    # Actually using python -m pip ... is better than the next line
    PYTHONPATH=/usr/local/Cellar/clingo/5.5.0/lib/python3.9/site-packages:$PYTHONPATH
    PYTHONPATH=./:$PYTHONPATH
    export PYTHONPATH
}


#Caffe Stuff:
#export DYLD_FALLBACK_LIBRARY_PATH=/usr/local/cuda/lib:$HOME/.pyenv/versions/anaconda-2.2.0/lib:/usr/local/lib/:/usr/lib

#NLTK:
export NLTK_DATA=~/assets/nlg/nltk
