#!/bin/bash

serv=unas.lan

mount_nas() {
  local pids=() mpoints=() mounts=()
  mapfile -t mounts < <(showmount -e "$serv" | awk 'NR>1 {print $1}')

  for shared in "${mounts[@]}"; do
    mpoint=${shared##*/}
    [[ ! -d /nas/$mpoint ]] && mkdir -p "/nas/$mpoint"
    if findmnt "/nas/$mpoint" &>/dev/null; then
      echo "→ /nas/$mpoint already mounted"
      continue
    fi
    echo "mounting $mpoint…"
    sudo mount -t nfs -o vers=3,timeo=600,retrans=5,_netdev,noatime "$serv:$shared" "/nas/$mpoint" &
    pids+=($!)
    mpoints+=("$mpoint")
  done

  for i in "${!pids[@]}"; do
    wait "${pids[$i]}" && echo "✓ /nas/${mpoints[$i]}" || echo "✗ /nas/${mpoints[$i]} failed"
  done
}

umount_nas() {
  local pids=() mpoints=()

  while IFS= read -r mpoint; do
    echo "unmounting $mpoint…"
    sudo umount "$mpoint" &
    pids+=($!)
    mpoints+=("$mpoint")
  done < <(findmnt -t nfs -o TARGET --noheadings)

  for i in "${!pids[@]}"; do
    wait "${pids[$i]}" && echo "✓ ${mpoints[$i]}" || echo "✗ ${mpoints[$i]} failed"
  done
}

mounted_nas() {
  local targets
  mapfile -t targets < <(findmnt -t nfs -o TARGET --noheadings)
  if [[ ${#targets[@]} -eq 0 ]]; then
    echo "∅"
  else
    printf '%s\n' "${targets[@]}"
  fi
}

choices=('mount' 'umount' 'mounted')
action=$(printf "%s\n" "${choices[@]}" |
  fzf --no-info --header="uNAS")

if [ -z "$action" ]; then
  exit 0
fi

case $action in
"mount") mount_nas ;;
"umount") umount_nas ;;
"mounted") mounted_nas ;;
esac
