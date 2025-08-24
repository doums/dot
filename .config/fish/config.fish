# aliases
alias ls='exa -lag --group-directories-first'
alias la='ls'
alias rt='trash'
alias emu='emulator -avd main -gpu host -accel on -no-boot-anim'
alias fk='fk.sh'
alias fkp='fkp.sh'
alias pac='pac.sh'
alias hok='hock.sh'
alias ts='ts.sh'
alias nv='nvim'
alias gs='git status'
alias gl='gl.sh'
alias gd='gd.sh'
alias glf='git log -p --date=format:%c --abbrev-commit --'
alias dk='docker'
alias dc='docker compose'
alias log='docker logs -f'
alias gu='gitui'
alias hx='helix'
alias x='xplr'

# disable that
set -g fish_greeting
# see https://github.com/fish-shell/fish-shell/issues/11204
# set -Ua fish_features no-keyboard-protocols

# env vars
# see https://wiki.archlinux.org/title/Debuginfod
set -x DEBUGINFOD_URLS https://debuginfod.archlinux.org
# set -x GDK_SCALE 2
# set -x GDK_DPI_SCALE 0.5
set -x SSH_AUTH_SOCK /run/user/1000/gcr/ssh

# nnn
set -x NNN_OPTS QUAuex
set -x NNN_COLORS 2341
set -x NNN_PLUG 'x:!ouch d -A "$nnn";X:!ouch ls "$nnn";i:!wezterm imgcat "$nnn"'
set -x NNN_BMS 'h:~;d:~/Documents/dot;r:/;m:/run/media/pierre;n:~/.config/nvim;x:/opt/xmonad;s:~/sync'
set -x NNN_FCOLORS '0505040a00060e0801030301'

# fzf
set -x FZF_DEFAULT_OPTS "--bind 'ctrl-w:toggle-preview-wrap,ctrl-p:toggle-preview,ctrl-a:toggle-all,alt-j:preview-half-page-down,alt-k:preview-half-page-up' --color='bg:-1,fg:-1,preview-bg:-1,preview-fg:-1,border:black:bold,bg+:#323232,fg+:-1:regular,hl:magenta:bold,hl+:magenta:bold,gutter:-1,pointer:yellow,prompt:blue,marker:green,spinner:bright-cyan,info:cyan:italic,header:blue:bold,query:-1:bold' --pointer=❱ --marker=❯ --prompt='❯ '"
set -x FZF_DEFAULT_COMMAND "fd -H -t f --strip-cwd-prefix"

# Java
set -x JAVA_HOME /usr/lib/jvm/default

# Go
set -x GOPATH $HOME/.go
set -x GOBIN $GOPATH/bin
set -x PATH $GOBIN $PATH

# bun
set -x BUN_INSTALL "$HOME/.bun"
set -x PATH $BUN_INSTALL/bin $PATH

# LanguageTool server
set -x LANGTOOL_HOST http://loup.lan:8010

# Nodejs
set -x PATH /opt/node22/bin $PATH

# manpage
set -x MANPAGER 'nvim +Man!'
set -x MANWIDTH 66

set -x _ZO_ECHO 1
zoxide init fish | source
