# Defined in - @ line 2
function fish_title
  set path (string split -r -m1 / $SHELL)
  if test (count $path) -gt 1
    echo $path[2]
  else
    echo $SHELL
  end
end
