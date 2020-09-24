
#------------------------------------------------------------
#antlr stuff
export CLASSPATH=/usr/local/lib/antlr-4.5-complete.jar:$CLASSPATH
alias antlr4='java -jar /usr/local/lib/antlr-4.5.3-complete.jar'
alias grun='java org.antlr.v4.runtime.misc.TestRig'
alias pant='antlr4 -Dlanguage=Python2'
alias antjs='antlr4 -Dlanguage=JavaScript'
#------------------------------------------------------------

#Python:
export JG_PYLIBS=~/github/.installed_pylibs

function set_non_standard_python_paths(){
    echo "Setting Non-Standard Python Paths"
    # Actually using python -m pip ... is better than the next line
    # export PYTHONPATH=/usr/local/lib/python3.7/site-packages # where pip3 installs
    PYTHONPATH=~/github/otherLibs # personally installed libs
    PYTHONPATH=~/github/:$PYTHONPATH # personally written libs
    PYTHONPATH=$JG_PYLIBS:$PYTHONPATH
    PYTHONPATH=./:$PYTHONPATH
    export PYTHONPATH
    #source activate root --not used because it slows default startup
}

#Caffe Stuff:
#export DYLD_FALLBACK_LIBRARY_PATH=/usr/local/cuda/lib:$HOME/.pyenv/versions/anaconda-2.2.0/lib:/usr/local/lib/:/usr/lib

#For Tidal:
export TIDAL_TEMPO_PORT=57120

#JavaScript:
#removed --harmony_destructuring
#alias test="node --harmony /usr/local/bin/nodeunit"
alias jsrepl="env NODE_NO_READLINE=1 node"
npm set prefix ~/.npm-global

#TEX:
export TEXINPUTS=/Volumes/DOCUMENTS/Dropbox/Scripts/tex/:$TEXINPUTS

#PERL
PERL5LIB=~/programming/perl/modules
PERL5LIB=~/Desktop/cotillion/packages/prompter/prompter:$PERL5LIB
PERL5LIB=~/Desktop/cotillion/packages/prompter/prompter/mod_aspects:$PERL5LIB
PERL5LIB=~/Desktop/cotillion/packages/prompter/prompter/mod_drama:$PERL5LIB
PERL5LIB=~/Desktop/cotillion/packages/prompter/prompter/mod_control:$PERL5LIB
PERL5LIB=~/Desktop/cotillion/packages/prompter/prompter/mod_services:$PERL5LIB
export PERL5LIB

#Prompter:
alias prompter="perl ~/Desktop/cotillion/packages/prompter/prompter/prompter.pl"


#NLTK:
export NLTK_DATA=~/assets/nlg/nltk

#Default editor:
export EDITOR=emacs

#TWINE:
alias twine="open ~/dropbox/Programs/Twine\ 2.0/index.html"

#JACAMO
export JACAMO_HOME=~/github/otherLibs/jacamo/build
export JDK_HOME="$(/usr/libexec/java_home)"
export JAVA_HOME="$(/usr/libexec/java_home)"

