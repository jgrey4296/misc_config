#!/usr/bin/env bash

function jgdebug () {
    # a debug message that only prints when JGDEBUG is true
    if [[ -n "${JGDEBUG-}" ]]; then
        echo "%% " $@
    fi
}

jgdebug Debug is "${JGDEBUG-}"


function jg_maybe_inc_prompt {
    # Increment the shell level each time you go into a subshell
    if [[ -n "$PROMPT_NUM" ]] && [[ $PROMPT_NUM -eq $PROMPT_NUM ]] 2> /dev/null; then
        jgdebug Prompt Level: $PROMPT_NUM
        PROMPT_NUM=$(($PROMPT_NUM + 1))
    else
        PROMPT_NUM=1
    fi
    jgdebug Depth Prompt: "${DEPTH_PROMPT-}"
}

function jg_prompt_update {
    #setting up the prompt:
    # from https://unix.stackexchange.com/questions/216953
    MAYBE_CONDA=""
    MAYBE_JAVA=""
    MAYBE_TMUX=""
    DEPTH_PROMPT="${PROMPT_NUM-1}"
    if [ "${PROMPT_NUM-}" -lt 2 ]; then
        DEPTH_PROMPT="⟘"
    fi
    JGPATH=$(pwd | sed -r 's/.+?\/(.+?\/.+?)/...\/\1/')

    if [[ -n "${CONDA_DEFAULT_ENV-}" ]]; then
        MAYBE_CONDA="py:${CONDA_DEFAULT_ENV-}"
    fi

    if [[ -n "$TMUX" ]]; then
        MAYBE_TMUX="TMUX:$(basename $TMUX)"
    fi

    }

function jg_set_prompt {
    # Set the term prompt with useful info
    PROMPT_COMMAND='jg_prompt_update'
    # Also modified in .condarc
    PS1='  ${MAYBE_TMUX} | u:\u | j:\j | $MAYBE_JAVA | $MAYBE_CONDA |- $JGPATH[$DEPTH_PROMPT]: '
}

function randname (){
    # get a random name, for tmux session names
    cat /usr/share/dict/words | shuf | head -n 1 | sed "s/'//g"
}

function loginmux () {
    # tmux aware session creation
    if [[ -n $TMUX  ]]; then
        return
    fi
    case "$TERM_PROGRAM" in
   	    tmux) return ;;
   	    emacs) return ;;

    esac

    if { tmux has-session; }; then
   	    echo "Tmux Session Running"
   	    tmux new-session -s `randname`
    else
   	 echo "No Tmux Session"
     tmux new-session -s `randname`
    fi
}

function attach () {
    # a simple tmux attach shortcut
    case "$TERM_PROGRAM" in
        tmux) return ;;
        emacs) return ;;
        *) tmux attach ;;
    esac
}

if [[ -z "$SU_WHITELIST" ]]; then
    SU_WHITELIST="TMUX,TERM_PROGRAM,PROMPT_NUM,TMUX_PANE"
    SU_WHITELIST="GNOME_SHELL_SESSION_MODE,XDG_CURRENT_DESKTOP,GNOME_TERMINAL_SCREEN,$SU_WHITELIST"
    SU_WHITELIST="GNOME_TERMINAL_SERVICE,GNOME_SETUP_DISPLAY,$SU_WHITELIST"
fi

case "$USER" in
    john)
        subu () {
            # a su cmd that preserves tmux info
            su -P -l --whitelist-environment="$SU_WHITELIST" jg
        }
    ;;
    *) ;;
esac
