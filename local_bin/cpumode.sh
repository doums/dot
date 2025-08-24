#!/bin/bash

set -e

# $1 - true | false
function set_turbo() {
  if [ "$1" == true ]; then
    echo 0 | sudo tee /sys/devices/system/cpu/intel_pstate/no_turbo >/dev/null
    echo "✓ turbo boost enabled"
  else
    echo 1 | sudo tee /sys/devices/system/cpu/intel_pstate/no_turbo >/dev/null
    echo "✓ turbo boost disabled"
  fi
}

# $1 - performance | powersave
# cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_available_governors
function set_governor() {
  governor=$(cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor)
  echo "current governor: $governor"
  sudo cpupower frequency-set --governor "$1"
  governor=$(cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor)
  echo "✓ updated governor [$governor]"
}

# $1 - performance | balance_performance | balance_power | power
# cat /sys/devices/system/cpu/cpu0/cpufreq/energy_performance_available_preferences
function set_epp() {
  epp=$(cat /sys/devices/system/cpu/cpu0/cpufreq/energy_performance_preference)
  echo "current EPP: $epp"
  sudo x86_energy_perf_policy --hwp-epp "$1"
  epp=$(cat /sys/devices/system/cpu/cpu0/cpufreq/energy_performance_preference)
  echo "✓ updated EPP [$epp]"
}

# $1 - performance | balance_performance | normal | balance_power | power
#      → 0         | 4                   | 6      | 8             | 15
# cat /sys/devices/system/cpu/cpu0/power/energy_perf_bias
#
# https://wiki.archlinux.org/title/CPU_frequency_scaling#Intel_performance_and_energy_bias_hint
function set_epb() {
  epb=$(cat /sys/devices/system/cpu/cpu0/power/energy_perf_bias)
  echo "current EPB: $epb"
  sudo x86_energy_perf_policy --epb "$1"
  epb=$(cat /sys/devices/system/cpu/cpu0/power/energy_perf_bias)
  echo "✓ updated EPB [$epb]"
}

function current() {
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
  echo "EPB: $epb"
}

modes="current\nperformance\nbalance\npowersave\ncold"
mode=$(echo -e "$modes" | fzf --header="CPU mode")
if [ -z "$mode" ]; then
  exit 0
fi

case $mode in
"current") current ;;
"performance")
  set_turbo true
  set_governor performance
  set_epp balance-performance
  set_epb 4 # balance-performance
  ;;
"balance")
  set_turbo true
  set_governor powersave
  set_epp balance-performance
  set_epb 6 # normal
  ;;
"powersave")
  set_turbo false
  set_governor powersave
  set_epp balance-power
  set_epb 8 # balance-power
  ;;
"cold")
  set_turbo false
  set_governor powersave
  set_epp power
  set_epb 15 # power
  ;;
esac
