#!/usr/bin/env bash

jgdebug "Setting up web paths"
APPLICATIONS="/Applications/"
APP_INTERNAL="Contents/MacOS"
    

case "$OSTYPE" in 
    darwin*)
    # Chrome
    PATH="$APPLICATIONS/Google Chrome.app/$APP_INTERNAL":$PATH
    # Firefox
    PATH="$APPLICATIONS/Firefox.app/$APP_INTERNAL":$PATH
    # Safari
    PATH="$APPLICATIONS/Safari.app/$APP_INTERNAL":$PATH
    # Tor
    PATH="$APPLICATIONS/Tor.app/$APP_INTERNAL":$PATH
    ;;
esac

