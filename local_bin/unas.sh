#!/bin/bash

serv=unas.lan
volume=/volume1
nasdir=/nas

exports=(
  # TODO
)

mount_nas() {
  local pids=() res=()

  sudo -v || return 1

  if [ ! -d "$nasdir" ]; then
    echo "creating $nasdir…"
    sudo mkdir -p "$nasdir"
  fi

  for shared in "${exports[@]}"; do
    local mpoint="$nasdir/$shared"
    [[ ! -d $mpoint ]] && sudo mkdir -p "$mpoint"
    if findmnt "$mpoint" &>/dev/null; then
      echo "→ $mpoint already mounted"
      continue
    fi
    echo "mounting $mpoint…"
    sudo mount -t nfs -o vers=3,timeo=600,retrans=5,noatime "$serv:$volume/$shared" "$mpoint" &
    pids+=($!)
    res+=("$mpoint")
  done

  for i in "${!pids[@]}"; do
    wait "${pids[$i]}" && echo "✓ ${res[$i]}" || echo "✗ ${res[$i]} failed"
  done
}

umount_nas() {
  local pids=() res=()

  sudo -v || return 1

  for shared in "${exports[@]}"; do
    local mpoint="$nasdir/$shared"
    if ! findmnt "$mpoint" &>/dev/null; then
      continue
    fi
    echo "unmounting $mpoint…"
    sudo umount "$mpoint" &
    pids+=($!)
    res+=("$mpoint")
  done

  for i in "${!pids[@]}"; do
    wait "${pids[$i]}" && echo "✓ ${res[$i]}" || echo "✗ ${res[$i]} failed"
  done
}

mounted_nas() {
  local targets=()
  for shared in "${exports[@]}"; do
    findmnt "$nasdir/$shared" &>/dev/null && targets+=("$nasdir/$shared")
  done
  if [ ${#targets[@]} -eq 0 ]; then
    echo "∅"
  else
    printf '%s\n' "${targets[@]}"
  fi
}

choices=('mount' 'umount' 'mounted' 'showmount')
action=$(printf "%s\n" "${choices[@]}" |
  fzf --no-info --header="uNAS")

if [ -z "$action" ]; then
  exit 0
fi

case $action in
"mount") mount_nas ;;
"umount") umount_nas ;;
"mounted") mounted_nas ;;
"showmount") timeout --kill-after=1s 5s showmount -e "$serv" ;;
esac
