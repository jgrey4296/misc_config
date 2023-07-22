jgdebug Setting Initial Path
BREW_PREFIX="/usr/local"

# PATHS:
PATH="/bin:/sbin"                           # Core
PATH="/usr/bin:/usr/sbin:/usr/libexec:$PATH"             # Secondary
PATH="/usr/local/bin:/usr/local/sbin:$PATH" # Tertiary

PATH="$HOME/.local/bin:$PATH" # local binaries

PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"
PATH="/usr/local/opt/make/libexec/gnubin:$PATH"
PATH="/usr/local/opt/findutils/libexec/gnubin:$PATH"

jgdebug Setting sqlite
PATH="/usr/local/opt/sqlite/bin:$PATH"

# MAN Paths
# https://www.howtogeek.com/682871/how-to-create-a-man-page-on-linux/
MANPATH="/usr/local/man:/usr/local/share/man:/usr/share/man"
MANPATH="$HOME/.doom.d/terminal/man/main:$MANPATH"
# MANPATH=/Library/Apple/usr/share/man:$MANPATH

# ANACONDA_HOME=/usr/local/anaconda3
ANACONDA_HOME="/usr/local/Caskroom/mambaforge/base"
ANACONDA_ENVS="${ANACONDA_HOME}/envs"
