
jgdebug "Setting Up Godot"
PATH=/Applications/Godot.app/Contents/MacOS:$PATH

# alias gscript="godot --headless --script"
# alias gslint="godot --headless --check-only --script"


function gdscript () {
    godot --headless -s $@
    osascript -e "tell application \"iTerm\"" -e "activate" -e "end tell"

}

export -f gdscript
