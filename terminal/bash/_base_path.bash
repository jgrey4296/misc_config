jgdebug Setting Initial Path
# PATHS:
PATH=/sbin:/usr/sbin:/bin:/usr/bin
PATH=/usr/local/sbin:$PATH
PATH=/usr/local/bin:$PATH
PATH=/usr/local/opt/coreutils/libexec/gnubin:$PATH
PATH=/usr/local/opt/make/libexec/gnubin:$PATH
PATH=/usr/local/opt/findutils/libexec/gnubin:$PATH

jgdebug Setting sqlite
PATH=/usr/local/opt/sqlite/bin:$PATH

jgdebug Setting calibre
PATH=/Applications/calibre.app/Contents/MacOS:$PATH

jgdebug Setting blender
PATH=/Applications/blender/blender.app/Contents/MacOS:$PATH

# MAN Paths
# https://www.howtogeek.com/682871/how-to-create-a-man-page-on-linux/
MANPATH=/usr/share/man:usr/local/share/man:/usr/local/man
MANPATH="$HOME/.doom.d/terminal/man/main":$MANPATH
# MANPATH=/Library/Apple/usr/share/man:$MANPATH
