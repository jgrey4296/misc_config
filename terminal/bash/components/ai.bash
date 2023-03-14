
jgd Setting Soar
export SOAR_HOME="$HOME/github/MAS/soar-cli"

jgd Setting CLIPS
export CLIPS_HOME="$HOME/github/MAS/clips"

jgd Setting Ceptre
export CEPTRE_HOME="$HOME/github/MAS/ceptre"

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
JACAMO_HOME="$HOME/github/MAS/jacamo/build"

jgd Setting Prompter
alias prompter="perl /Volumes/documents/github/MAS/cotillion/packages/prompter/prompter/prompter.pl"

JG_AI="$JDK_HOME/bin:$JASON_HOME/scripts:$JACAMO_HOME/scripts"

PATH="$JG_AI:$PATH"
