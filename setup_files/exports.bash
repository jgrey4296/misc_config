#PATHS:
export PATH=/sbin:/usr/sbin:/bin:/usr/bin
export PATH=/usr/local/sbin:$PATH
export PATH=/usr/local/bin:$PATH
export PATH=/Applications/SuperCollider:$PATH
export PATH=/usr/local/opt/coreutils/libexec/gnubin:$PATH
export PATH=/usr/local/go/bin:$PATH

# Doom
export PATH=~/.emacs.d/bin/:$PATH

#Node:
export PATH=~/.npm-global/bin:$PATH

#For supercollider:
export PATH=/Applications/SuperCollider/SuperCollider.app/Contents/Resources:$PATH
export PATH=/Applications/SuperCollider/SuperCollider.app/Contents/MacOS:$PATH

#sqlite
export PATH=/usr/local/opt/sqlite/bin:$PATH

#for latex (mactex, when installed):
# export PATH=/usr/local/texlive/2017/bin/x86_64-darwin:$PATH

#for blender:
export PATH=/Applications/blender/blender.app/Contents/MacOS:$PATH

#for jacamo (see lang specific)
export PATH=$JACAMO_HOME/scripts:$PATH

#for ruby / gems:
export PATH=/usr/local/opt/ruby/bin:$PATH
export PATH="$(gem environment gemdir)/bin":$PATH

#setting up the prompt:
export PS1='(\j): \D{%D} \A \u:  '

#Setting the location for spacemacs:
# export SPACEMACSDIR=~/.spacemacs.d/

#for gtags:
export GTAGSCONF=~/.shell_files/gtags.conf
export GTAGSLABEL=pygments

#Shell Location update
export SHELL="$(which bash)"

