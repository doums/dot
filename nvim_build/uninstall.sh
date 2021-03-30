#!/bin/bash

share_dir="/usr/share/nvim"
lib_dir="/usr/lib/nvim"
to_remove=()
run=0

mapfile -t files < "build/install_manifest.txt"
for file in "${files[@]}"; do
  if [ -n "${file##${share_dir}*}" ] && [ -n "${file##${lib_dir}*}" ]; then
    to_remove+=("$file")
  fi
done

printf "%s\n" "$share_dir"
printf "%s\n" "$lib_dir"
printf "%s\n" "${to_remove[@]}"

printf "\n"
read_input=1
while [ $read_input -eq 1 ]; do
  read -r -s -N 1 -p "remove these files? (y/n) " input
  printf "\n"
  if [ "$input" = "y" ]; then
    read_input=0
    run=1
  elif [ "$input" == "n" ]; then
    read_input=0
    printf "cancelled\n"
  fi
done

if [ $run -eq 1 ]; then
  sudo rm -r "$share_dir"
  printf "%s removed\n" "$share_dir"
  sudo rm -r "$lib_dir"
  printf "%s removed\n" "$lib_dir"
  for file in "${to_remove[@]}"; do
    sudo rm "$file"
    printf "%s removed\n" "$file"
  done
  printf "\ndone\n"
fi
