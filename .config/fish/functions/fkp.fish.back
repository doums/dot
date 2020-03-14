# fast search for a process using a given port and kill it
function fkp
  set processes (ss -pnOatu | fzf -m --header-lines=1 --preview 'echo {}' --preview-window=up:4:wrap | awk '{print $7}')
  for elem in $processes
    set pid (string match -r 'pid=(\d*),' $elem)
    set pid $pid[2]
    if test -n $pid
      if not kill $pid > /dev/null 2>&1
        sudo kill $pid
      end
    end
  end
end

