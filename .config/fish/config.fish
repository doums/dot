# aliases
alias ls='exa -lag --group-directories-first'
alias la='ls'
alias db='adb shell input keyevent 82'
alias dbk='adb shell am force-stop com.monbuilding.app'
alias rr='adb shell input text "RR"'
alias ds='xrandr --output eDP-1 --auto --output DP-2 --auto --scale 1.333 --right-of eDP-1'
alias fk='fk.sh'
alias fkp='fkp.sh'
alias pac='pac.sh'
alias hok='hock.sh'
alias vim='nvim'
alias gs='git status'
alias gl='gl.sh'
alias gd='gd.sh'
alias glf='git log -p --date=format:%c --abbrev-commit --'
alias wtr='curl fr.wttr.in'
alias dk='docker'
alias dc='docker compose'
alias log='docker logs -f'
alias gu='gitui'

# bindings
bind \eh '~; commandline -f repaint'
bind \ed '~/Documents/dotfiles/; commandline -f repaint'
bind \ev '~/.config/nvim/; commandline -f repaint'
bind \ex '/opt/xmonad/; commandline -f repaint'
bind \en '~/Documents/nym/nym/; commandline -f repaint'

# env vars
set fish_greeting
set -x fish_emoji_width 2
set -x GDK_SCALE 2
set -x GDK_DPI_SCALE 0.5

# Nodejs
# set -x PATH /opt/node16/bin $PATH

