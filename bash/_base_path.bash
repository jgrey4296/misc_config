jgdebug Setting Initial Path

export JG_CACHE="$HOME/.cache"
export JG_CONFIG="$HOME/.config/jg"
export GH_CONFIG_DIR="$JG_CONFIG/gh"

PATH="/jg_path"

case "$OSTYPE" in
    darwin*) 
        BREW_PREFIX="/usr/local"
        ;;
    linux*) 
        PATH="/snap/bin:$PATH"
        PATH="/usr/local/games:$PATH"

esac
PATH="/bin:/sbin:$PATH"                                  # Core
PATH="/usr/bin:/usr/sbin:/usr/libexec:$PATH"             # Secondary
PATH="/usr/local/bin:/usr/local/sbin:$PATH"              # Tertiary

PATH="$HOME/.local/bin:$PATH"                            # local binaries

case "$OSTYPE" in
    darwin*)
        PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"
        PATH="/usr/local/opt/make/libexec/gnubin:$PATH"
        PATH="/usr/local/opt/findutils/libexec/gnubin:$PATH"
        jgdebug Setting sqlite
        PATH="/usr/local/opt/sqlite/bin:$PATH"
        ;;
    linux*)
        ;;
esac

# MAN Paths
# https://www.howtogeek.com/682871/how-to-create-a-man-page-on-linux/
MANPATH="/usr/local/man:/usr/local/share/man:/usr/share/man"
MANPATH="$HOME/.config/jg/templates/man/main:$MANPATH"

INFOPATH=""
