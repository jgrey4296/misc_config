#!/usr/bin/env bash

# Android Path
JG_AP=~/Library/Android/sdk
JG_FP=$JG_AP/tools/bin
JG_FP=$JG_AP/platform-tools:$JG_FP
JG_FP=$JG_AP/build-tools/30.0.3:$JG_FP
JG_FP=/Applications/Android\ Studio.app/Contents/plugins/Kotlin/kotlinc/bin:$JG_FP

export PATH=$JG_FP:$PATH
# Android java paths
export ANDROID_JDK=/Applications/Android\ Studio.app/Contents/jre/Contents/Home/bin
# Android jar libs
export ANDROID_LIB=/Applications/Android\ Studio.app/Contents/lib/
export ANDROID_JARS=/Applications/Android\ Studio.app/Contents/plugins/android/lib/.
export ANDROID_SDK=/Users/johngrey/Library/Android/sdk/platforms/android-32/
