#!/usr/bin/env bash

jgdebug "Adding Supercollider to path"
PATH=/Applications/SuperCollider:$PATH
#For supercollider:
PATH=/Applications/SuperCollider/SuperCollider.app/Contents/Resources:$PATH
PATH=/Applications/SuperCollider/SuperCollider.app/Contents/MacOS:$PATH

jgdebug "adding csound for python bindings"
DYLD_FRAMEWORK_PATH="/usr/local/opt/csound/Frameworks":"${DYLD_FRAMEWORK_PATH-}"

jgdebug "setting Tidal port"
TIDAL_TEMPO_PORT=57120
