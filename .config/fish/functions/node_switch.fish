function node_switch
  set -l _version $argv[1]
  set -l red (set_color -o red)
  set -l white (set_color -o white)
  set -l green (set_color -o green)
  set -l normal (set_color normal)

  if test -z $_version
    printf 'expect version as argument %s✕%s\n' $red $normal
    return 1
  end
  if ! test -e /opt/node$_version/bin/node
    printf 'invalid version %s%s%s %s✕%s\n' $white $_version $normal $red $normal
    return 1
  end
  sed -i "s/^set -x PATH \/opt\/node.\+\/bin \$PATH/set -x PATH \/opt\/node$_version\/bin \$PATH/" "$HOME/.config/fish/config.fish"
  set -x PATH /opt/node$_version/bin $PATH
  printf 'switched to version %s%s %s✓%s\n' $white $_version $green $normal
  printf 'node: %s%s%s\n' $white (node -v) $normal
  printf 'npm:  %s%s%s\n' $white (npm -v) $normal
end
