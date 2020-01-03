# a fast docker manager using fzf

function manage_container
  set containers (docker ps -a | fzf -m --header-lines=1 | awk '{print $1}')
  if test (count $containers) -eq 0
    return 0
  end
  set action (printf 'inspect\nkill\nlogs\npause\nport\nrestart\nrm\nstart\nstop\ntop\nunpause\nupdate\nwait\n' | fzf)
  if test -z $action
    return 0
  end
  set cmd docker
  switch $action
    case inspect kill pause restart rm start stop unpause update wait
      set -a cmd $action
      for id in $containers
        set -a cmd $id
      end
    case logs port top
      set -a cmd $action $containers[1]
  end
  eval $cmd
end


function manage_image
  set images (docker images | fzf -m --header-lines=1 | awk '{print $3}')
  if test (count $images) -eq 0
    return 0
  end
  set action (printf 'history\ninspect\nsave\nrm' | fzf)
  if test -z $action
    return 0
  end
  set cmd 'docker image'
  switch $action
    case inspect save rm
      set -a cmd $action
      for id in $images
        set -a cmd $id
      end
    case history
      set -a cmd history $images[1]
  end
  eval $cmd
end

function manage_volume
  set volumes (docker volume ls | fzf -m --header-lines=1 | awk '{print $2}')
  if test (count $volumes) -eq 0
    return 0
  end
  set action (printf 'inspect\nrm' | fzf)
  if test -z $action
    return 0
  end
  set cmd "docker volume $action"
  for id in $volumes
    set -a cmd $id
  end
  eval $cmd
end

function fdocker
  if not docker ps > /dev/null
    return 1
  end
  set choice (printf 'container\nimage\nvolume\n' | fzf)
  if test -z $choice
    return 0
  end
  switch $choice
    case container
      manage_container
    case image
      manage_image
    case volume
      manage_volume
  end
end

