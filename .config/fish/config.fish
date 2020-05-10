if test -n "$DESKTOP_SESSION"
    set (gnome-keyring-daemon --start | string split "=")
end

alias ..='cd ..'
alias ls='ls --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias ll='ls -l --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias la='ls -la --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias grep='grep --color=tty -d skip'
alias sb='subl'
alias db='adb shell input keyevent 82'
alias dbk='adb shell am force-stop com.monbuilding.app'
alias rr='adb shell input text "RR"'
alias ds='xrandr --output DP-2 --auto --scale 1.333 --above eDP-1'
alias fk='fk.sh'
alias fkp='fkp.sh'
alias pac='pac.sh'
alias vim='nvim'

set fish_greeting
set -x LD_LIBRARY_PATH /usr/local/lib $LD_LIBRARY_PATH
set -x QT_AUTO_SCREEN_SCALE_FACTOR 1
set -x GDK_SCALE 2
set -x GDK_DPI_SCALE 0.5
set -x ANDROID_HOME $HOME/Android/Sdk
set -x PATH $ANDROID_HOME/emulator $PATH
set -x PATH $ANDROID_HOME/tools $PATH
set -x PATH $ANDROID_HOME/tools/bin $PATH
set -x PATH $ANDROID_HOME/platform-tools $PATH
set -x JAVA_HOME /usr/lib/jvm/default
set -x XDG_SESSION_TYPE X11
set -x PATH $PATH $HOME/.cargo/bin
set -x PATH $PATH $HOME/.gem/ruby/2.7.0/bin
set -x fish_emoji_width 2
set -x PATH /opt/node/bin $PATH
set -x PATH /home/pierre/.yarn/bin $PATH
set -x PATH $HOME/.local/bin $PATH
set -x PATH $PATH $HOME/.local/share/JetBrains/Toolbox/bin
set -x MANPAGER 'nvim +Man!'
set -x MAKEFLAGS '-j8'
set -x BROWSER none
set -x FZF_DEFAULT_OPTS "--bind 'ctrl-w:toggle-preview-wrap,ctrl-p:toggle-preview' --color='bg:#2e3440,fg:#eceff4,preview-bg:#2e3440,preview-fg:#eceff4,border:#d8dee9,bg+:#434c5e,fg+:#eceff4,hl:#8fbcbb,hl+:#88c0d0,gutter:#3b4252,pointer:#e5e9f0,prompt:#88c0d0,marker:#81a1c1,spinner:#81a1c1,header:#5e81ac,info:#81a1c1'"
set -x FZF_DEFAULT_COMMAND "rg --files --hidden --no-ignore"
set -x XDG_DATA_DIRS /home/pierre/.local/share/flatpak/exports/share:/var/lib/flatpak/exports/share:/usr/local/share:/usr/share
