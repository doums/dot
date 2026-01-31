#!/bin/bash

set -E
set -o pipefail

# ANSI style codes
RED="\e[38;5;1m" # red
MGT="\e[38;5;5m" # magenta
BLD="\e[1m"      # bold
ITL="\e[3m"      # italic
RS="\e[0m"       # style reset
B_RED="$BLD$RED"
B_MGT="$BLD$MGT"
BI_MGT="$ITL$B_MGT"
####

trap 'catch_err $? $LINENO' ERR

LAPTOP_OUTPUT="eDP-1"
declare -A monitors
connected_outputs=()
# known monitor specs
DUALUP_MODE=2560x2880
DUALUP_FREQ=60
DUALUP_OUTPUT=
AW_MODE=2560x1440
AW_FREQ=360
AW_OUTPUT=
BENQ_MODE=1920x1080
BENQ_FREQ=240
BENQ_OUTPUT=

# shellcheck disable=SC2329
catch_err() {
  trap - ERR
  echo -e " $B_RED✗$RS unexpected error, [$BLD$1$RS] L#$BLD$2$RS"
  exit 1
}

log() {
  echo -e "$1"
}

trim() {
  local var="$*"
  var="${var#"${var%%[![:space:]]*}"}"
  var="${var%"${var##*[![:space:]]}"}"
  printf '%s' "$var"
}

# $1 float number
ceil() {
  intval=${1%%.*}
  fractional=${1#*.}
  if [[ "$1" == *.* &&
    "$fractional" -ne 0 &&
    "$1" != "$intval" ]]; then
    intval=$((intval + 1))
  fi
  echo "$intval"
}

# switch on/off wlan0 and keyboard backlight
# $1: on | off
dock_mode() {
  if [ "$1" == 'on' ]; then
    if iwctl device wlan0 show | rg -q 'Powered\s+on'; then
      iwctl device wlan0 set-property Powered off
      echo 'wlan0 switched off'
    fi
    brightnessctl -d tpacpi::kbd_backlight set 0 &>/dev/null
  else
    if iwctl device wlan0 show | rg -q 'Powered\s+off'; then
      iwctl device wlan0 set-property Powered on
      echo 'wlan0 switched on'
    fi
    brightnessctl -d tpacpi::kbd_backlight set 1 &>/dev/null
  fi
}

setup_laptop() {
  opts=(--output "$LAPTOP_OUTPUT" --primary --auto)
  turn_off=()
  for output in "${connected_outputs[@]}"; do
    if [ "$output" != "$LAPTOP_OUTPUT" ]; then
      echo "turning off $output"
      turn_off+=(--output "$output" --off)
    fi
  done
  xrandr "${opts[@]}" "${turn_off[@]}"

  dock_mode 'off'
}

# BenQ primary, DualUp on left
benq_dualup() {
  xrandr --output "$LAPTOP_OUTPUT" --off \
    --output "$DUALUP_OUTPUT" --auto --pos 0x0 \
    --output "$BENQ_OUTPUT" --primary --mode 1920x1080 --rate 240 --pos 2560x1260

  dock_mode 'on'
}

# AW primary, DualUp on left
aw_dualup() {
  xrandr --output "$LAPTOP_OUTPUT" --off \
    --output "$DUALUP_OUTPUT" --auto --pos 0x0 \
    --output "$AW_OUTPUT" --primary --mode "$AW_MODE" --rate 240 --pos 2560x1260

  dock_mode 'on'
}

# laptop primary, DualUp on left
laptop_dualup() {
  xrandr --output "$LAPTOP_OUTPUT" --primary --auto --pos 2560x1000 \
    --output "$DUALUP_OUTPUT" --auto --pos 0x0
}

# BenQ primary, laptop on left
benq_laptop() {
  xrandr --output "$LAPTOP_OUTPUT" --auto --pos 0x0 \
    --output "$BENQ_OUTPUT" --primary --mode "$BENQ_MODE" --rate "$BENQ_FREQ" --pos 2880x0
}

# AW primary, laptop on left
aw_laptop() {
  xrandr --output "$LAPTOP_OUTPUT" --auto --pos 0x0 \
    --output "$AW_OUTPUT" --primary --mode "$AW_MODE" --rate 240 --pos 2880x0
}

select_layout() {
  dualup="$DUALUP_OUTPUT"
  aw="$AW_OUTPUT"
  benq="$BENQ_OUTPUT"

  fzf_status=0
  choices=('laptop')
  if [ -n "$benq" ] && [ -n "$dualup" ]; then
    choices+=("BenQ & DualUp")
  fi
  if [ -n "$aw" ] && [ -n "$dualup" ]; then
    choices+=("AW & DualUp")
  fi
  if [ -n "$aw" ]; then
    choices+=("AW & laptop")
  fi
  if [ -n "$benq" ]; then
    choices+=("BenQ & laptop")
  fi
  if [ -n "$dualup" ]; then
    choices+=("laptop & DualUp")
  fi
  choice=$(printf "%s\n" "${choices[@]}" |
    fzf --no-info --header="setup display") || fzf_status=$?

  if [[ $fzf_status -eq 130 ]] || [[ -z "$choice" ]]; then
    exit 0
  fi

  case "$choice" in
  "laptop")
    setup_laptop
    ;;
  "BenQ & DualUp")
    benq_dualup
    ;;
  "AW & DualUp")
    aw_dualup
    ;;
  "AW & laptop")
    aw_laptop
    ;;
  "BenQ & laptop")
    benq_laptop
    ;;
  "laptop & DualUp")
    laptop_dualup
    ;;
    # TODO cfg DualUp
  *) exit 1 ;;
  esac
}

# $1 xrandr mode line output
# expected format: <WIDTHxHEIGHT>[_REFRESHRATE] <REFRESHRATE1>[*][+] [<REFRESHRATE2> ...]
parse_mode_and_rates() {
  IFS=' ' read -r mode rest <<<"$1"
  mode=$(trim "${mode}")
  # then split rates into array, removing markers
  IFS=' ' read -ra arr <<<"${rest//[+*]/ }"
  highest_rate=0
  for rate in "${arr[@]}"; do
    rate=$(trim "$rate")
    rate=$(ceil "$rate")
    if [ -z "$highest_rate" ]; then
      highest_rate=$rate
    elif [ "$rate" -gt "$highest_rate" ]; then
      highest_rate=$rate
    fi
  done
  echo "$mode:$highest_rate"
}

parse_xrandr() {
  mapfile -t xmonitors < <(xrandr |
    rg -A1 '\bconnected' |
    tr '\n' '|' |
    sed 's/|$//' |
    sed 's/|--|/\n/g')
  # entries have this format:
  # <OUTPUT_NAME> connected [primary] [<CURRENT_RESOLUTION>+<X>+<Y>] (...)| <SUPPORTED_RES_AND_RATES>
  for line in "${xmonitors[@]}"; do
    output=${line%% *}
    connected_outputs+=("$output")
    [[ $line =~ ([0-9]+x[0-9]+\+[0-9]+\+[0-9]+) ]] && state=1 ||
      state=0
    layout="${BASH_REMATCH[1]:--}"
    res_data=$(trim "${line##*|}")
    mode_rate=$(parse_mode_and_rates "$res_data")
    mode=${mode_rate%%:*}
    highest_rate=${mode_rate##*:}
    # echo "[$output] state:$state layout:$layout mode:$mode highest_rate:$highest_rate"

    monitors["state:$output"]="$state"
    monitors["layout:$output"]="$layout"
    monitors["mode:$output"]="${mode:--}"
    monitors["rate:$output"]="$highest_rate"
  done
}

match_known_monitors() {
  for key in "${!monitors[@]}"; do
    if [[ $key == mode:* ]]; then
      output=${key#mode:}
      mode=${monitors[$key]}
      rate=${monitors["rate:$output"]}
      if [ "$mode" == "$DUALUP_MODE" ] && [ "$rate" -eq "$DUALUP_FREQ" ]; then
        echo "[$output] detected DualUp mode $mode at ${rate}Hz"
        DUALUP_OUTPUT=$output
      elif [ "$mode" == "$AW_MODE" ] && [ "$rate" -eq "$AW_FREQ" ]; then
        echo "[$output] detected AW2725DF mode $mode at ${rate}Hz"
        AW_OUTPUT=$output
      elif [ "$mode" == "$BENQ_MODE" ] && [ "$rate" -eq "$BENQ_FREQ" ]; then
        echo "[$output] detected BenQ XL2546X mode $mode at ${rate}Hz"
        BENQ_OUTPUT=$output
      elif [ "$output" != "$LAPTOP_OUTPUT" ]; then
        echo "[$output] unknown monitor mode $mode at ${rate}Hz"
      fi
    fi
  done
}

log "${BI_MGT}parsing xrandr…${RS}"
parse_xrandr
match_known_monitors
select_layout
log "${MGT}*${BI_MGT}DONE${RS}${MGT}*"
exit 0
