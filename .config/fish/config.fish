# aliases
alias ls='exa -lag --group-directories-first'
alias la='ls'
alias emu='emulator -avd main -gpu host -accel on -no-boot-anim'
alias fk='fk.sh'
alias fkp='fkp.sh'
alias pac='pac.sh'
alias hok='hock.sh'
alias nv='nvim'
alias gs='git status'
alias gl='gl.sh'
alias gd='gd.sh'
alias glf='git log -p --date=format:%c --abbrev-commit --'
alias wtr='curl fr.wttr.in'
alias dk='docker'
alias dc='docker compose'
alias log='docker logs -f'
alias gu='gitui'
alias hx='helix'

# bindings
bind \eh '~; commandline -f repaint'
bind \ed '~/Documents/dot/; commandline -f repaint'
bind \ev '~/.config/nvim/; commandline -f repaint'
bind \ex '/opt/xmonad/; commandline -f repaint'
bind \en '~/Documents/nym/nym/; commandline -f repaint'

# env vars
# see https://wiki.archlinux.org/title/Debuginfod
set -x DEBUGINFOD_URLS https://debuginfod.archlinux.org
# set -x GDK_SCALE 2
# set -x GDK_DPI_SCALE 0.5

# Java
set -x JAVA_HOME /usr/lib/jvm/default

# android dev
set -x ANDROID_HOME $HOME/.local/share/android/sdk
set -x ANDROID_USER_HOME $HOME/.local/share/android/user
set -x PATH $ANDROID_HOME/tools $PATH
set -x PATH $ANDROID_HOME/emulator $PATH
set -x PATH $ANDROID_HOME/tools/bin $PATH
set -x PATH $ANDROID_HOME/platform-tools $PATH
set -x NDK_HOME $ANDROID_HOME/ndk/25.2.9519653

# LanguageTool server
set -x LANGTOOL_HOST http://loup:8010
