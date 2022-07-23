#!/usr/bin/env bash

# Soar
SOAR_HOME=~/github/MAS/soar-cli
alias soar="$SOAR_HOME/SoarCLI.sh"
PATH=$SOAR_HOME:$PATH

# CLIPS
CLIPS_HOME=~/github/MAS/clips
alias clips="$CLIPS_HOME/CLIPS\ Console"
PATH=$CLIPS_HOME:$PATH

#ceptre
CEPTRE_HOME=~/github/MAS/ceptre
# alias ceptre="$CEPTRE_HOME/bin/ceptre"
PATH=$CEPTRE_HOME/bin:$PATH

#JACAMO
JACAMO_HOME=~/github/MAS/jacamo/build
JDK_HOME="$(/usr/libexec/java_home)"
JAVA_HOME="$(/usr/libexec/java_home)"
#for jacamo (see lang specific)
PATH=$JACAMO_HOME/scripts:$PATH

#Prompter:
alias prompter="perl /Volumes/documents/github/MAS/cotillion/packages/prompter/prompter/prompter.pl"
