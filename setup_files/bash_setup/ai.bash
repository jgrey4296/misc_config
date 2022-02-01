#!/usr/bin/env bash

# Soar
alias soar="SoarCLI.sh"

# CLIPS
alias clips="CLIPS\ Console"

#Soar
PATH=~/github/soar-cli:$PATH

#CLIPS
PATH=~/github/clips/:$PATH

#ceptre
PATH=/Volumes/documents/github/ceptre/bin:$PATH

#JACAMO
export JACAMO_HOME=/Volumes/documents/github/jacamo/build
export JDK_HOME="$(/usr/libexec/java_home)"
export JAVA_HOME="$(/usr/libexec/java_home)"
#for jacamo (see lang specific)
export PATH=$JACAMO_HOME/scripts:$PATH

#Prompter:
alias prompter="perl /Volumes/documents/github/cotillion/packages/prompter/prompter/prompter.pl"
