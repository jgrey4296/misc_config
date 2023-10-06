
jgdebug "Setting Up Godot"

case "$OSTYPE" in 
    darwin*)
        PATH=/Applications/Godot.app/Contents/MacOS:$PATH
        ;;
    linux*)

        ;;
esac

# alias gscript="godot --headless --script"
# alias gslint="godot --headless --check-only --script"


function gdscript () {
    godot --headless -s $@
    osascript -e "tell application \"iTerm\"" -e "activate" -e "end tell"

}

export -f gdscript
