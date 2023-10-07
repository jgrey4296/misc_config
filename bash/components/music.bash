#!/usr/bin/env bash

jgdebug "Adding Supercollider to path"
case "$OSTYPE" in
    darwin*)
        PATH=/Applications/SuperCollider:$PATH
        #For supercollider:
        PATH=/Applications/SuperCollider/SuperCollider.app/Contents/Resources:$PATH
        PATH=/Applications/SuperCollider/SuperCollider.app/Contents/MacOS:$PATH
        jgdebug "adding csound for python bindings"
        DYLD_FRAMEWORK_PATH="/usr/local/opt/csound/Frameworks":"${DYLD_FRAMEWORK_PATH-}"
        ;;
esac


jgdebug "setting Tidal port"
TIDAL_TEMPO_PORT=57120
