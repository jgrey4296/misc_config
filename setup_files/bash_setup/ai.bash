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

jgd Setting Java
# JDK_HOME="${HOME}/.gradle/jdks/adoptium-19-x64-hotspot-mac/Contents/Home"
JDK_HOME="/usr/local/opt/openjdk@17/libexec/openjdk.jdk/Contents/Home"
JAVA_HOME=$JDK_HOME
PATH=$JDK_HOME/bin:$PATH

jgd Setting Gradle
alias gradle="/usr/local/Cellar/gradle/7.5.1_1/libexec/bin/gradle"
alias gra="./gradlew -q"


jgd Setting Jason
JASON_HOME=~/github/MAS/jason/build
PATH=$JASON_HOME/scripts:$PATH

jgd Setting JACAMO
JACAMO_HOME=~/github/MAS/jacamo/build
PATH=$JACAMO_HOME/scripts:$PATH




jgd Setting Prompter
alias prompter="perl /Volumes/documents/github/MAS/cotillion/packages/prompter/prompter/prompter.pl"
