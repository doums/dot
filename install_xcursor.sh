#!/bin/bash

# install the XCursor theme
# https://github.com/ful1e5/XCursor-pro
# and swap some icons

# requires `go-yq`

set -e

icondir="/usr/share/icons"
repo=ful1e5/XCursor-pro
theme=XCursor-Pro-Dark
temp_dir=$(mktemp -d)

pushd "$temp_dir"
echo "fetching latest release…"
latest=$(curl -SsfL \
  -H "Accept: application/vnd.github+json" \
  -H "X-GitHub-Api-Version: 2022-11-28" \
  https://api.github.com/repos/$repo/releases/latest | yq .tag_name)
echo "downloading $theme $latest…"
curl -fLO -# https://github.com/$repo/releases/download/"$latest/$theme".tar.xz
echo "extracting…"
tar -xvf $theme.tar.xz

echo "patching cursors…"
pushd "$theme/cursors"
rm "fleur"
# replace the <^v> cursor with the closed hand (grabbing)
ln -s "move" "fleur"
# replace double arrow bar <|> with simple double arrow <->
h_icons=(
  'sb_h_double_arrow'
  'h_double_arrow'
  'col-resize'
  'ew-resize'
  'size-hor'
  'size_hor'
  'split_h'
)
# same for vertical arrows
v_icons=(
  'sb_v_double_arrow'
  'v_double_arrow'
  'row-resize'
  'ns-resize'
  'size-ver'
  'size_ver'
  'split_v'
  'double_arrow'
)
for icon in "${h_icons[@]}"; do
  rm "$icon"
  ln -s "left_side" "$icon"
done
for icon in "${v_icons[@]}"; do
  rm "$icon"
  ln -s "top_side" "$icon"
done
popd

if [ -d "$icondir/$theme" ]; then
  echo "removing $icondir/$theme"
  sudo rm -rf "$icondir/${theme:?}"
fi
echo "installing $theme to $icondir"
sudo mv "$theme" "$icondir/$theme"
popd
rm -rf "$temp_dir"

echo "installed to $icondir/$theme"
echo "✓ DONE"
