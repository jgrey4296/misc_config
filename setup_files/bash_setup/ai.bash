#!/usr/bin/env bash

# Soar
SOAR_HOME=~/github/soar-cli
alias soar="$SOAR_HOME/SoarCLI.sh"

# CLIPS
CLIPS_HOME=~/github/clips
alias clips="$CLIPS_HOME/CLIPS\ Console"

#ceptre
CEPTRE_HOME=~/github/ceptre
alias ceptre="$CEPTRE_HOME/bin/ceptre"

#JACAMO
JACAMO_HOME=~/github/jacamo/build
JDK_HOME="$(/usr/libexec/java_home)"
JAVA_HOME="$(/usr/libexec/java_home)"
#for jacamo (see lang specific)
PATH=$JACAMO_HOME/scripts:$PATH

#Prompter:
alias prompter="perl /Volumes/documents/github/cotillion/packages/prompter/prompter/prompter.pl"
