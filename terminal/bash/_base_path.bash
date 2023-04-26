jgdebug Setting Initial Path
BREW_PREFIX=/usr/local

# PATHS:
PATH=/usr/bin:/usr/sbin:/bin:/sbin        # Core
PATH=/usr/bin:/usr/sbin:$PATH             # Secondary
PATH=/usr/local/bin:/usr/local/sbin:$PATH # Tertiary

PATH=/usr/local/opt/coreutils/libexec/gnubin:$PATH
PATH=/usr/local/opt/make/libexec/gnubin:$PATH
PATH=/usr/local/opt/findutils/libexec/gnubin:$PATH

jgdebug Setting sqlite
PATH=/usr/local/opt/sqlite/bin:$PATH


# MAN Paths
# https://www.howtogeek.com/682871/how-to-create-a-man-page-on-linux/
MANPATH=/usr/local/man:/usr/local/share/man:/usr/share/man
MANPATH="$HOME/.doom.d/terminal/man/main":$MANPATH
# MANPATH=/Library/Apple/usr/share/man:$MANPATH

ANACONDA_HOME=/usr/local/anaconda3
