#!/usr/bin/env bash

function jgdebug () {
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

#setting up the prompt:
# from https://unix.stackexchange.com/questions/216953
function jg_prompt_update {
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
    PROMPT_COMMAND='jg_prompt_update'
    # Also modified in .condarc
    PS1='  ${MAYBE_TMUX} | u:\u | j:\j | $MAYBE_JAVA | $MAYBE_CONDA |- $JGPATH[$DEPTH_PROMPT]: '
}

randname (){
   cat /usr/share/dict/words | shuf | head -n 1 | sed "s/'//g"
}

loginmux () {
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

attach () {
  case "$TERM_PROGRAM" in
      tmux) return ;;
      emacs) return ;;
  esac

  tmux attach
}

