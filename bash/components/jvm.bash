#!/usr/bin/env bash
# https://developer.android.com/tools/variables
# Required sdkmanager packages:
# Android SDK Build Tools
# Android SDK Platform Tools
# Android SDK Command-Line Tools
# Android SDK Platform
# Google USB Driver
#
# https://sdkman.io
#
# android sdk home is deprecated, use android_home

jgdebug "Setting JVM"
BUILD_TOOLS="33.0.1"

case "$OSTYPE" in
    darwin*)
        STUDIO_HOME="/Applications/Android\ Studio.app/contents"
        ANDROID_HOME="$HOME/Library/Android/sdk"
        ANDROID_USER_HOME="$HOME/.android"
        ADB_VENDOR_KEYS="$HOME/.android"
        ANDROID_LIB="$STUDIO_HOME/lib/"
        ANDROID_JARS="$STUDIO_HOME/plugins/android/lib/"
        ANDROID_SDKS="$ANDROID_HOME/platforms/"
        STUDIO_JDK="$STUDIO_HOME/jbr/Contents/Home/"
        ANDROID_TOOLS="$ANDROID_HOME/bin"
        ;;
    linux*)
        STUDIO_HOME="/snap/android-studio/current"
        # ANDROID_HOME="/usr/lib/android-sdk"
        ANDROID_HOME="$HOME/android"
        ANDROID_USER_HOME="$BASE_CACHE/android"
        ADB_VENDOR_KEYS="$BASE_CONFIG/secrets/android"
        ANDROID_TOOLS="$ANDROID_HOME/cmdline-tools/latest/bin"
        ANDROID_TOOLS="$ANDROID_HOME/build-tools/$BUILD_TOOLS/bin:$ANDROID_TOOLS"
        ANDROID_TOOLS="$ANDROID_HOME/platform-tools:$ANDROID_TOOLS"
        ;;
esac


jgdebug "Setting up SDKMAN"
SDKMAN_DIR="$BASE_CACHE/sdkman"
if [[ -e "$SDKMAN_DIR/bin/sdkman-init.sh" ]]; then
        source "$SDKMAN_DIR/bin/sdkman-init.sh"
fi

jgdebug "Setting Gradle"
if [[ -d "$BASE_CACHE/gradle" ]]; then
        GRADLE_USER_HOME="$BASE_CACHE/gradle"
fi

jgdebug "Setting Jason"
JASON_HOME="$HOME/github/.local/jason-3.2.2"

jgdebug "Setting JACAMO"
JACAMO_HOME="$HOME/.local/javamo-1.2.2"

PATH="$ANDROID_TOOLS:$JDK_HOME/bin:$PATH"
