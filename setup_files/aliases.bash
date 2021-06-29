#PERSONAL ALIASES:
#------------------------------------------------------------
alias emacs="TERM=xterm-24bits emacs"

#ssh
alias mini="ssh jg@192.168.1.5"
alias ubun="ssh john@192.168.1.11"
alias wind="ssh john@192.168.1.2"

alias pServer="python -m http.server 8888 &"
alias qpServer="(python -m http.server 8888 > /dev/null 2>&1) & && (echo 'Running Quiet Server')"
alias qpoServer="((python -m http.server 8888 > /dev/null 2>&1) &) && (echo 'Running Quiet Server') && (open http://localhost:8888/)"

alias jges="emacs ~/.shell_files/"

alias em="emacs"
alias lsa="ls -lha"
alias lsd="gls -la --group-directories-first"
alias lsda="ls -la | grep '^d'"

alias ql="qlmanage -p 2>/dev/null"

alias cd="mycd $1"
alias cdc="mycdc $1"
alias cda="mycda $1"

function mycd(){
    builtin cd "$@" && ls
}

function mycdc(){
    builtin cd "$@" && lsc
}

function mycda(){
    builtin cd "$@" && lsa
}

alias jgf="find . -maxdepth 1 -iname $1"

#disk usage:
alias jgdu="du -hd 1 | sort"

#sclang for emacs:
alias sclangel="sclang -d ~/github/.super_collider_classes/ -r -s -i emacs"

#for automating the update of pip packages:
alias pipupdate="pip list --outdated --format=freeze | grep -v '^\-e' | cut -d = -f 1 | xargs -n1 pip install -U"

#Emacs aliases:
alias temacs="emacs ~/github/writing/TODO.org"
alias wemacs="emacs ~/github/writing/main.org"

#haskell:
#don't forget to update emacs haskell program name variable when modifying this
alias whaskell="ghci -Wall -fwarn-name-shadowing -ghci-script ~/.shell_files/.ghci_alt"
alias haskell="ghci -ghci-script ~/.shell_files/.ghci_alt"
alias hs="haskell"

#Disable osx gatekeeper: (needs sudo)
alias gatekeeper="spctl --master-disable"

# Soar
alias soar="SoarCLI.sh"
