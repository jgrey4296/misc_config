
jgd Setting Soar
SOAR_HOME="$HOME/github/MAS/soar-cli"
alias soar="$SOAR_HOME/SoarCLI.sh"

jgd Setting CLIPS
CLIPS_HOME="$HOME/github/MAS/clips"
alias clips="$CLIPS_HOME/CLIPS\ Console"

jgd Setting Ceptre
CEPTRE_HOME="$HOME/github/MAS/ceptre"
# alias ceptre="$CEPTRE_HOME/bin/ceptre"

jgd Setting Java
# JDK_HOME="${HOME}/.gradle/jdks/adoptium-19-x64-hotspot-mac/Contents/Home"
JDK_HOME="/usr/local/opt/openjdk@17/libexec/openjdk.jdk/Contents/Home"
JAVA_HOME="$JDK_HOME"

jgd Setting Gradle
alias gradle="/usr/local/Cellar/gradle/7.5.1_1/libexec/bin/gradle"
alias gra="./gradlew -q"

jgd Setting Jason
JASON_HOME="$HOME/github/MAS/jason/build"

jgd Setting JACAMO
JACAMO_HOME=~/github/MAS/jacamo/build


jgd Setting Prompter
alias prompter="perl /Volumes/documents/github/MAS/cotillion/packages/prompter/prompter/prompter.pl"

JG_AI="$SOAR_HOME:$CLIPS_HOME:$CEPTRE_HOME/bin:$JDK_HOME/bin:$JASON_HOME/scripts:$JACAMO_HOME/scripts"

PATH="$JG_AI:$PATH"
