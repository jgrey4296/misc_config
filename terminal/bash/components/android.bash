#!/usr/bin/env bash
# https://developer.android.com/tools/variables
# Required sdkmanager packages:
# Android SDK Build Tools
# Android SDK Platform Tools
# Android SDK Command-Line Tools
# Android SDK Platform
# Google USB Driver


jgdebug "Setting Android"

STUDIO_HOME="/Applications/Android\ Studio.app/contents"
ANDROID_HOME="$HOME/Library/Android/sdk"
ANDROID_USER_HOME="$HOME/.android"

# Android tools paths
ANDROID_TOOL_PATHS="$ANDROID_HOME/cmdline-tools/latest/bin"
ANDROID_TOOL_PATHS="$ANDROID_HOME/platform-tools:$ANDROID_TOOL_PATHS"
ANDROID_TOOL_PATHS="$ANDROID_HOME/build-tools/33.0.0:$ANDROID_TOOL_PATHS"
# ANDROID_TOOL_PATHS="$ANDROID_HOME/cmdline-tools/{version}/bin:$ANDROID_TOOL_PATHS"

# Android java paths
ANDROID_LIB="$studio_home/lib/"
ANDROID_JARS="$studio_home/plugins/android/lib/"
ANDROID_SDKS="$HOME/Library/Android/sdk/platforms/"
STUDIO_JDK="$STUDIO_HOME/jbr/Contents/Home/"
# STUDIO_GRADLE_JDK

jgdebug "Setting Kotlin"


jgdebug "Setting Java"
# JDK_HOME="${HOME}/.gradle/jdks/adoptium-19-x64-hotspot-mac/Contents/Home"
JDK_HOME="$HOME/Library/Java/JavaVirtualMachines/openjdk-20.0.2/Contents/Home/"
JAVA_HOME="$JDK_HOME"

jgdebug "Setting Gradle"
# alias gradle="/usr/local/Cellar/gradle/7.5.1_1/libexec/bin/gradle"
# alias gra="./gradlew -q"

PATH="$ANDROID_TOOL_PATHS:$PATH"
