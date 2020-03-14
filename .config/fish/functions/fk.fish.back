# fast search for a process or a selection of processes and kill it
function fk
  set pids (ps -eo user=user,pid=pid,ppid=ppid,%cpu=cpu,%mem=mem,args=cmd | fzf -m --header-lines=1 --preview 'echo {}' --preview-window=up:4:wrap | awk '{print $2}')
  for pid in $pids
    if not kill $pid > /dev/null 2>&1
      sudo kill $pid
    end
  end
end

