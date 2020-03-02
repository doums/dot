# find a file fast
function ffd
  fd -H -t f -t l $argv | fzf --preview 'echo {}' --preview-window=down:4:wrap
end
