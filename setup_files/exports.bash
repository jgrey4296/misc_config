#PATHS:
PATH=/sbin:/usr/sbin:/bin:/usr/bin
PATH=/usr/local/sbin:$PATH
PATH=/usr/local/bin:$PATH
PATH=/Applications/SuperCollider:$PATH
PATH=/usr/local/opt/coreutils/libexec/gnubin:$PATH
PATH=/usr/local/go/bin:$PATH

# Doom
PATH=~/.emacs.d/bin/:$PATH

#Node:
PATH=~/.npm-global/bin:$PATH

#For supercollider:
PATH=/Applications/SuperCollider/SuperCollider.app/Contents/Resources:$PATH
PATH=/Applications/SuperCollider/SuperCollider.app/Contents/MacOS:$PATH

#sqlite
PATH=/usr/local/opt/sqlite/bin:$PATH

#for latex (mactex, when installed):
export PATH=/usr/local/texlive/2017/bin/x86_64-darwin:$PATH

#for blender:
PATH=/Applications/blender/blender.app/Contents/MacOS:$PATH

#for jacamo (see lang specific)
PATH=$JACAMO_HOME/scripts:$PATH

#for ruby / gems:
PATH=/usr/local/opt/ruby/bin:$PATH
PATH="$(gem environment gemdir)/bin":$PATH

#setting up the prompt:
PS1='(\j): \D{%D} \A \u:  '

#Setting the location for spacemacs:
# export SPACEMACSDIR=~/.spacemacs.d/

#for gtags:
GTAGSCONF=~/.shell_files/gtags.conf
GTAGSLABEL=pygments

#Shell Location update
SHELL="$(which bash)"

export PATH
