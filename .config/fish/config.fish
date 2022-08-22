alias ls='exa --group-directories-first'
alias la='exa -lag --group-directories-first'
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

set fish_greeting
set -x XDG_CONFIG_HOME $HOME/.config
set -x SSH_AUTH_SOCK /run/user/1000/keyring/ssh
set -x LD_LIBRARY_PATH /usr/local/lib $LD_LIBRARY_PATH
set -x QT_AUTO_SCREEN_SCALE_FACTOR 1
set -x EDITOR /usr/bin/nvim
set -x VISUAL /usr/bin/nvim
set -x JAVA_HOME /usr/lib/jvm/default
set -x XDG_SESSION_TYPE X11
set -x PATH $PATH $HOME/.cargo/bin
set -x PATH $PATH $HOME/.gem/ruby/2.7.0/bin
set -x fish_emoji_width 2
set -x PATH /opt/ltex-ls/bin $PATH
set -x PATH /home/pierre/.yarn/bin $PATH
set -x PATH $HOME/.local/bin $PATH
set -x PATH $PATH $HOME/.local/share/JetBrains/Toolbox/scripts
set -x PATH $PATH $HOME/.local/share/JetBrains/Toolbox/bin
set -x MANPAGER 'nvim +Man!'
set -x MAKEFLAGS '-j8'
set -x FZF_DEFAULT_OPTS "--bind 'ctrl-w:toggle-preview-wrap,ctrl-p:toggle-preview,ctrl-a:toggle-all,alt-j:preview-half-page-down,alt-k:preview-half-page-up' --color='bg:-1,fg:-1,preview-bg:-1,preview-fg:-1,border:black:bold,bg+:#323232,fg+:-1:regular,hl:magenta:bold,hl+:magenta:bold,gutter:-1,pointer:yellow,prompt:blue,marker:green,spinner:bright-cyan,info:cyan:italic,header:blue:bold,query:-1:bold' --pointer=❱ --marker=❯ --prompt='❯ '"
set -x FZF_DEFAULT_COMMAND "fd -HI -t f"
set -x XDG_DATA_DIRS /home/pierre/.local/share/flatpak/exports/share:/var/lib/flatpak/exports/share:/usr/local/share:/usr/share
set -x NNN_OPTS QAuex
set -x NNN_COLORS 2341
set -x NNN_PLUG 't:-_bat $nnn;f:-_|firefox $nnn*'
set -x NNN_BMS 'h:~;d:~/Documents/dotfiles;r:/;m:/run/media/pierre;v:~/.config/nvim;n:~/Documents/nym/nym'
set -x NNN_FCOLORS '0505040a00060e0801030301'
set -x DENO_INSTALL $HOME/.deno
set -x PATH $PATH $DENO_INSTALL/bin
set -x LIBVA_DRIVER_NAME vdpau

# Nodejs
set -x PATH /opt/node16/bin $PATH

