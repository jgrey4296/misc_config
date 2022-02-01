#!/usr/bin/env bash

# Android Path
JG_AP=~/Library/Android/sdk
# Android tools paths
JG_FP=$JG_AP/tools/bin
JG_FP=$JG_AP/platform-tools:$JG_FP
JG_FP=$JG_AP/build-tools/30.0.3:$JG_FP
# Kotlin
JG_FP=/Applications/Android\ Studio.app/Contents/plugins/Kotlin/kotlinc/bin:$JG_FP

PATH=$JG_FP:$PATH

# Android java paths
ANDROID_JDK=/Applications/Android\ Studio.app/Contents/jre/Contents/Home/bin
# Android jar libs
ANDROID_LIB=/Applications/Android\ Studio.app/Contents/lib/
ANDROID_JARS=/Applications/Android\ Studio.app/Contents/plugins/android/lib/.
ANDROID_SDK=/Users/johngrey/Library/Android/sdk/platforms/android-32/
