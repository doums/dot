#!/usr/bin/bash

# hock, Handy dOCKer manager using fzf

back_prompt() {
  choice=$(printf 'back\nquit' | fzf --height=4 --layout=default --no-info --no-mouse)
  if [ -z "$choice" ]; then
    return 0
  elif [ "$choice" = 'quit' ]; then
    return 1
  fi;
}

manage_container() {
  selection=$(docker ps -a | fzf -m --header-lines=1 --preview 'echo {}' --preview-window=up:4:wrap | awk '{print $1}')
  mapfile -t containers <<< "$selection"
  if [ ${#containers[*]} -eq 1 ] && [ -z "${containers[0]}" ]; then
    return 0
  fi
  action=$(printf 'inspect\nkill\nlogs\npause\nport\nrestart\nrm\nstart\nstop\ntop\nunpause\nupdate\nwait\n' | fzf --no-info)
  if [ -z "$action" ]; then
    if ! manage_container; then
      return 1
    fi
    return 0
  fi
  cmd=(docker)
  case "$action" in
    inspect | kill | pause | restart | rm | start | stop | unpause | update | wait)
      cmd+=("$action")
      for id in "${containers[@]}"; do
        cmd+=("$id")
      done
    ;;
    logs | port | top)
      cmd+=("$action" "${containers[0]}")
    ;;
  esac
  eval "${cmd[*]}"
  if ! back_prompt; then
    return 1
  fi
  manage_container
}

manage_image() {
  selection=$(docker images | fzf -m --header-lines=1 --preview 'echo {}' --preview-window=up:2:wrap | awk '{print $3}')
  mapfile -t images <<< "$selection"
  if [ ${#images[*]} -eq 1 ] && [ -z "${images[0]}" ]; then
    return 0
  fi
  action=$(printf 'history\ninspect\nsave\nrm' | fzf --no-info)
  if [ -z "$action" ]; then
    if ! manage_image; then
      return 1
    fi
    return 0
  fi
  cmd=(docker image)
  case "$action" in
    inspect | save | rm)
      cmd+=("$action")
      for id in "${images[@]}"; do
        cmd+=("$id")
      done
    ;;
    history)
      cmd+=('history' "${images[0]}")
    ;;
  esac
  eval "${cmd[*]}"
  if ! back_prompt; then
    return 1
  fi
  manage_image
}

manage_volume() {
  selection=$(docker volume ls | fzf -m --header-lines=1 --preview 'echo {}' --preview-window=up:2:wrap | awk '{print $2}')
  mapfile -t volumes <<< "$selection"
  if [ ${#volumes[*]} -eq 1 ] && [ -z "${volumes[0]}" ]; then
    return 0
  fi
  action=$(printf 'inspect\nrm' | fzf --no-info)
  if [ -z "$action" ]; then
    if ! manage_volume; then
      return 1
    fi
    return 0
  fi
  cmd=(docker volume "$action")
  for id in "${volumes[@]}"; do
    cmd+=("$id")
  done
  eval "${cmd[*]}"
  if ! back_prompt; then
    return 1
  fi
  manage_volume
}

manage_network() {
  selection=$(docker network ls | fzf -m --header-lines=1 --preview 'echo {}' --preview-window=up:2:wrap | awk '{print $2}')
  mapfile -t networks <<< "$selection"
  if [ ${#networks[*]} -eq 1 ] && [ -z "${networks[0]}" ]; then
    return 0
  fi
  action=$(printf 'inspect\nprune\nrm' | fzf --no-info)
  if [ -z "$action" ]; then
    if ! manage_network; then
      return 1
    fi
    return 0
  fi
  cmd=(docker network "$action")
  for id in "${networks[@]}"; do
    cmd+=("$id")
  done
  eval "${cmd[*]}"
  if ! back_prompt; then
    return 1
  fi
  manage_network
}

hock() {
  if ! systemctl is-active docker &> /dev/null; then
    choice=$(printf 'start docker ?\nyes\nno' | fzf --header-lines=1 --no-info)
    if [ -z "$choice" ]; then
      exit 0
    fi
    if [ "$choice" = 'yes' ]; then
      if ! sudo systemctl start docker; then
        exit 1
      fi
    else
      exit 0
    fi
  fi
  choice=$(printf 'container\nimage\nvolume\nnetwork\nstop docker' | fzf --no-info)
  if [ -z "$choice" ]; then
    exit 0
  fi
  case "$choice" in
    container)
      if ! manage_container; then
        return 0
      fi
      hock;;
    image)
      if ! manage_image; then
        return 0
      fi
      hock;;
    volume)
      if ! manage_volume; then
        return 0
      fi
      hock;;
    network)
      if ! manage_network; then
        return 0
      fi
      hock;;
    'stop docker')
      sudo systemctl stop docker
      exit 0;;
  esac
}

hock
