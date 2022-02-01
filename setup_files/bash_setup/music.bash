#!/usr/bin/env bash

PATH=/Applications/SuperCollider:$PATH
#For supercollider:
PATH=/Applications/SuperCollider/SuperCollider.app/Contents/Resources:$PATH
PATH=/Applications/SuperCollider/SuperCollider.app/Contents/MacOS:$PATH
# adding csound for python bindings
export DYLD_FRAMEWORK_PATH="$DYLD_FRAMEWORK_PATH:/usr/local/opt/csound/Frameworks"

#For Tidal:
export TIDAL_TEMPO_PORT=57120
