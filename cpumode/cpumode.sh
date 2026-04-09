#!/bin/bash

set -e

# function that check if the script is running as root
function is_root() {
  if [ "$EUID" -ne 0 ]; then
    echo "run with sudo"
    exit 1
  fi
}

# profiles
# array: turbo - governor - epp - epb
p_cold=(0 powersave power power)
p_balanced=(1 powersave balance-power balance-power)
p_perf=(1 powersave balance-performance balance-performance)

# $1 - 1 | 0
function set_turbo() {
  if [ "$1" == 1 ]; then
    echo 0 | tee /sys/devices/system/cpu/intel_pstate/no_turbo >/dev/null
    echo "✓ turbo boost enabled"
  else
    echo 1 | tee /sys/devices/system/cpu/intel_pstate/no_turbo >/dev/null
    echo "✓ turbo boost disabled"
  fi
}

# $1 - performance | powersave
# cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_available_governors
function set_governor() {
  if [ "$1" != "performance" ] && [ "$1" != "powersave" ]; then
    echo "invalid governor: $1"
    echo "valid options are: performance, powersave"
    exit 1
  fi
  governor=$(cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor)
  echo "prev governor: $governor"
  echo "$1" | tee /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor >/dev/null
  governor=$(cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor)
  echo "✓ updated governor [$governor]"
}

# $1 - performance | balance-performance | normal | balance-power | power
# note: normal = balance-performance -> 128
# see man x86_energy_perf_policy
# cat /sys/devices/system/cpu/cpu0/cpufreq/energy_performance_available_preferences
function set_epp() {
  epp=$(cat /sys/devices/system/cpu/cpu0/cpufreq/energy_performance_preference)
  echo "prev EPP: $epp"
  x86_energy_perf_policy --hwp-epp "$1"
  epp=$(cat /sys/devices/system/cpu/cpu0/cpufreq/energy_performance_preference)
  echo "✓ updated EPP [$epp]"
}

function epb_to_str() {
  case $1 in
  0) echo "performance" ;;
  4) echo "balance-performance" ;;
  6) echo "normal" ;;
  8) echo "balance-power" ;;
  15) echo "power" ;;
  *) echo "$1" ;;
  esac
}

# $1 - performance | balance-performance | normal | balance-power | power
#    → 0           | 4                   | 6      | 8             | 15
# cat /sys/devices/system/cpu/cpu0/power/energy_perf_bias
#
# https://wiki.archlinux.org/title/CPU_frequency_scaling#Intel_performance_and_energy_bias_hint
function set_epb() {
  epb=$(cat /sys/devices/system/cpu/cpu0/power/energy_perf_bias)
  echo "prev EPB: $(epb_to_str "$epb")"
  x86_energy_perf_policy --epb "$1"
  epb=$(cat /sys/devices/system/cpu/cpu0/power/energy_perf_bias)
  echo "✓ updated EPB [$(epb_to_str "$epb")]"
}

function show_current() {
  no_turbo=$(cat /sys/devices/system/cpu/intel_pstate/no_turbo)
  if [ "$no_turbo" -eq 0 ]; then
    echo "turbo: ON"
  else
    echo "turbo: OFF"
  fi
  governor=$(cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor)
  echo "governor: $governor"
  epp=$(cat /sys/devices/system/cpu/cpu0/cpufreq/energy_performance_preference)
  echo "EPP: $epp"
  epb=$(cat /sys/devices/system/cpu/cpu0/power/energy_perf_bias)
  echo "EPB: $(epb_to_str "$epb")"
}

function set_automode() {
  ac=$(cat /sys/class/power_supply/AC/online)
  if [ "$ac" -eq 1 ]; then
    echo "[AC] setting perf mode"
    setmode "${p_perf[@]}"
  else
    echo "[battery] setting balanced mode"
    setmode "${p_balanced[@]}"
  fi
}

function setmode() {
  set_turbo "$1"
  set_governor "$2"
  set_epp "$3"
  set_epb "$4"
}

function select_profile() {
  modes="cold\nbalanced\nperf"
  mode=$(echo -e "$modes" | fzf --header="CPU mode")
  if [ -z "$mode" ]; then
    exit 0
  fi

  case $mode in
  "perf")
    setmode "${p_perf[@]}"
    ;;
  "balanced")
    setmode "${p_balanced[@]}"
    ;;
  "cold")
    setmode "${p_cold[@]}"
    ;;
  esac
}

if [ "$#" -eq 0 ]; then
  show_current
  exit 0
fi

if [ "$1" == "select" ]; then
  is_root
  select_profile
  exit 0
fi

if [ "$1" == "auto" ]; then
  is_root
  set_automode
  exit 0
fi

if [ "$#" -eq 2 ] && [ "$1" == "set" ]; then
  is_root
  case "$2" in
  "perf")
    setmode "${p_perf[@]}"
    ;;
  "balanced")
    setmode "${p_balanced[@]}"
    ;;
  "cold")
    setmode "${p_cold[@]}"
    ;;
  *)
    echo "invalid mode: $2"
    exit 1
    ;;
  esac
  exit 0
fi

if [ "$#" -ne 4 ]; then
  echo "Usage: $(basename "$0") <turbo: 0|1> <governor: performance|powersave> <epp: LVL> <epb: LVL>"
  echo "  LVL: performance, balance-performance, normal, balance-power, power"
  echo "  note: for epp, normal = balance-performance (128)"
  exit 1
fi
is_root
setmode "$1" "$2" "$3" "$4"

echo "DONE"
exit 0
