#
# ~/.bashrc
#

#If not running interactively, don't do anything
[[ $- != *i* ]] && return

export PATH=$PATH:/opt/node/bin/
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib

alias ..='cd ..'
alias ls='ls --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias ll='ls -l --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias la='ls -la --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias grep='grep --color=tty -d skip'

# bash provides PROMPT_COMMAND to hold a command that will be evaluated right before PS1
# xterm enables to change the window icon and title with a special sequence using the Operating System Command
# xterm defines OSC as following: ESC ] (an escape follewed by a closing bracket)
# the corresponding sequence using by xterm to change the window icon and title is
# OSC 0 ; text BEL
# its ascii equivalent \033]0;text\007
# or with bash escape sequences \e]0;text\a
# with text as a text parameter composed of printable characters.
#
# sources:
# http://tldp.org/HOWTO/Xterm-Title-4.html#ss4.3
# http://invisible-island.net/xterm/ctlseqs/ctlseqs.html

# https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html

PROMPT_COMMAND='printf "\e]0;%s %s\a" "${HOSTNAME%%.*}" "${PWD/#$HOME/\~}"'
PS1='\u \[\033[0;32m\]\$ \[\033[m\]'

export QT_AUTO_SCREEN_SCALE_FACTOR=1
# export GDK_SCALE=2
# export GDK_DPI_SCALE=0.5

source "$HOME/.cargo/env"
