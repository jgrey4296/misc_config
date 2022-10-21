#!/usr/bin/env bash

jgd Setting Soar
SOAR_HOME=~/github/MAS/soar-cli
alias soar="$SOAR_HOME/SoarCLI.sh"
PATH=$SOAR_HOME:$PATH

jgd Setting CLIPS
CLIPS_HOME=~/github/MAS/clips
alias clips="$CLIPS_HOME/CLIPS\ Console"
PATH=$CLIPS_HOME:$PATH

jgd Setting Ceptre
CEPTRE_HOME=~/github/MAS/ceptre
# alias ceptre="$CEPTRE_HOME/bin/ceptre"
PATH=$CEPTRE_HOME/bin:$PATH

jgd Setting JACAMO
JACAMO_HOME=~/github/MAS/jacamo/build

jgd Setting Java
JDK_HOME="${HOME}/.gradle/jdks/adoptium-19-x64-hotspot-mac/Contents/Home"
JAVA_HOME="${HOME}/.gradle/jdks/adoptium-19-x64-hotspot-mac/Contents/Home"

PATH=$JACAMO_HOME/scripts:$JDK_HOME/bin:$PATH


jgd Setting Prompter
alias prompter="perl /Volumes/documents/github/MAS/cotillion/packages/prompter/prompter/prompter.pl"

export JDK_HOME
