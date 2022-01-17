
#------------------------------------------------------------
#antlr stuff
export CLASSPATH=/usr/local/lib/antlr-4.5-complete.jar:$CLASSPATH
alias antlr4='java -jar /usr/local/lib/antlr-4.5.3-complete.jar'
alias grun='java org.antlr.v4.runtime.misc.TestRig'
alias pant='antlr4 -Dlanguage=Python2'
alias antjs='antlr4 -Dlanguage=JavaScript'
#------------------------------------------------------------

#Python:

function set_non_standard_python_paths(){
    echo "Setting Non-Standard Python Paths"
    # Actually using python -m pip ... is better than the next line
    PYTHONPATH=/usr/local/Cellar/clingo/5.5.0/lib/python3.9/site-packages:$PYTHONPATH
    PYTHONPATH=./:$PYTHONPATH
    export PYTHONPATH
}

# adding csound for python bindings
export DYLD_FRAMEWORK_PATH="$DYLD_FRAMEWORK_PATH:/usr/local/opt/csound/Frameworks"

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
# export TEXINPUTS=/Volumes/DOCUMENTS/Dropbox/Scripts/tex/:$TEXINPUTS

#PERL
PERL5LIB=~/Desktop/cotillion/packages/prompter/prompter:$PERL5LIB
PERL5LIB=~/Desktop/cotillion/packages/prompter/prompter/mod_aspects:$PERL5LIB
PERL5LIB=~/Desktop/cotillion/packages/prompter/prompter/mod_drama:$PERL5LIB
PERL5LIB=~/Desktop/cotillion/packages/prompter/prompter/mod_control:$PERL5LIB
PERL5LIB=~/Desktop/cotillion/packages/prompter/prompter/mod_services:$PERL5LIB
export PERL5LIB

#Prompter:
alias prompter="perl /Volumes/documents/github/cotillion/packages/prompter/prompter/prompter.pl"

#NLTK:
export NLTK_DATA=~/assets/nlg/nltk

#Default editor:
export EDITOR=emacs

#JACAMO
export JACAMO_HOME=~/github/jacamo/build
export JDK_HOME="$(/usr/libexec/java_home)"
export JAVA_HOME="$(/usr/libexec/java_home)"

# Android Path
JG_AP=~/Library/Android/sdk
JG_FP=$JG_AP/tools/bin
JG_FP=$JG_AP/platform-tools:$JG_FP
JG_FP=$JG_AP/build-tools/30.0.3:$JG_FP
JG_FP=/Applications/Android\ Studio.app/Contents/plugins/Kotlin/kotlinc/bin:$JG_FP

export PATH=$JG_FP:$PATH
# Android java paths
export ANDROID_JDK=/Applications/Android\ Studio.app/Contents/jre/Contents/Home/bin
# Android jar libs
export ANDROID_LIB=/Applications/Android\ Studio.app/Contents/lib/
export ANDROID_JARS=/Applications/Android\ Studio.app/Contents/plugins/android/lib/.
export ANDROID_SDK=/Users/johngrey/Library/Android/sdk/platforms/android-32/
