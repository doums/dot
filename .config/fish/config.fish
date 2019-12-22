alias ..='cd ..'
alias ls='ls --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias ll='ls -l --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias la='ls -la --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias grep='grep --color=tty -d skip'
alias sb='subl'
alias rp='/usr/bin/./remove_package.sh'
alias emu="cd /home/pierre/Android/Sdk/emulator/;emulator -avd android -gpu host -accel on -scale 2 -no-boot-anim"
alias db="adb shell input keyevent 82"
alias dbk="adb shell am force-stop com.monbuilding.app"
alias rr="adb shell input text "RR""
alias ds="xrandr --output eDP-1 --auto --output DP-2 --auto --scale 1.33333x1.33333 --right-of eDP-1"

set fish_greeting
set -x LD_LIBRARY_PATH /usr/local/lib $LD_LIBRARY_PATH
set -x QT_AUTO_SCREEN_SCALE_FACTOR 1
set -x GDK_SCALE 2
set -x GDK_DPI_SCALE 0.5
set -x ANDROID_HOME $HOME/Android/Sdk
set -x JAVA_HOME /usr/lib/jvm/default
set -x XDG_SESSION_TYPE X11
set -x PATH /opt/arcanist/bin $PATH
set -x PATH /home/pierre/Android/Sdk/platform-tools $PATH
set -x PATH $HOME/.cargo/env $PATH
set -x fish_emoji_width 2
set -x PATH /opt/node/bin $PATH
set -x PATH /home/pierre/.yarn/bin $PATH
set -x MANPAGER "vim -M +MANPAGER -"
set -x MAKEFLAGS "-j8"
set -x BROWSER none
set -x FZF_DEFAULT_COMMAND "rg --files --hidden --no-ignore --follow"
