#!/usr/bin/env bash
# https://developer.android.com/tools/variables
# Required sdkmanager packages:
# Android SDK Build Tools
# Android SDK Platform Tools
# Android SDK Command-Line Tools
# Android SDK Platform
# Google USB Driver
# https://github.com/jenv/jenv for environment management

jgdebug "Setting JVM"

STUDIO_HOME="/Applications/Android\ Studio.app/contents"
ANDROID_HOME="$HOME/Library/Android/sdk"
ANDROID_USER_HOME="$JG_CACHE/.android"

# Android tools paths
ANDROID_TOOL_PATHS="$ANDROID_HOME/cmdline-tools/latest/bin"
ANDROID_TOOL_PATHS="$ANDROID_HOME/platform-tools:$ANDROID_TOOL_PATHS"
ANDROID_TOOL_PATHS="$ANDROID_HOME/build-tools/33.0.0:$ANDROID_TOOL_PATHS"
# ANDROID_TOOL_PATHS="$ANDROID_HOME/cmdline-tools/{version}/bin:$ANDROID_TOOL_PATHS"

# Android java paths
ANDROID_LIB="$STUDIO_HOME/lib/"
ANDROID_JARS="$STUDIO_HOME/plugins/android/lib/"
ANDROID_SDKS="$ANDROID_HOME/platforms/"
STUDIO_JDK="$STUDIO_HOME/jbr/Contents/Home/"
# STUDIO_GRADLE_JDK

jgdebug "Setting Java"
# JDK_HOME="${HOME}/.gradle/jdks/adoptium-19-x64-hotspot-mac/Contents/Home"
JDK_HOME="/usr/local/opt/openjdk/libexec/openjdk.jdk/Contents/Home"
JAVA_HOME="$JDK_HOME"

jgdebug "Setting Kotlin"

jgdebug "Setting Gradle"
GRADLE_USER_HOME="$JG_CACHE/gradle"

jgdebug "Setting Jason"
JASON_HOME="$HOME/github/jvm/__jason/build"

jgdebug "Setting JACAMO"
JACAMO_HOME="$HOME/github/jvm/__jacamo/build"

JG_JACAMO_PATHS="$JASON_HOME/scripts:$JACAMO_HOME/scripts"

PATH="$ANDROID_TOOL_PATHS:$JG_JACAMO_PATHS:$JDK_HOME/bin:$PATH"
