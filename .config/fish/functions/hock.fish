# hock, Handy dOCKer manager using fzf

function continue_prompt
  set choice (printf 'back\nquit' | fzf --height=4 --layout=default --no-info --no-mouse)
  if test -z $choice
    return 0
  end
  if test $choice = 'quit'
    return 1
  end
end

function manage_container
  set containers (docker ps -a | fzf -m --header-lines=1 | awk '{print $1}')
  if test (count $containers) -eq 0
    return 0
  end
  set action (printf 'inspect\nkill\nlogs\npause\nport\nrestart\nrm\nstart\nstop\ntop\nunpause\nupdate\nwait\n' | fzf --no-info)
  if test -z $action
    manage_container
    if test $status -eq 1
      return 1
    end
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
  continue_prompt
  if test $status -eq 1
    return 1
  end
  manage_container
end

function manage_image
  set images (docker images | fzf -m --header-lines=1 | awk '{print $3}')
  if test (count $images) -eq 0
    return 0
  end
  set action (printf 'history\ninspect\nsave\nrm' | fzf --no-info)
  if test -z $action
    manage_image
    if test $status -eq 1
      return 1
    end
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
  continue_prompt
  if test $status -eq 1
    return 1
  end
  manage_image
end

function manage_volume
  set volumes (docker volume ls | fzf -m --header-lines=1 | awk '{print $2}')
  if test (count $volumes) -eq 0
    return 0
  end
  set action (printf 'inspect\nrm' | fzf --no-info)
  if test -z $action
    manage_volume
    if test $status -eq 1
      return 1
    end
    return 0
  end
  set cmd "docker volume $action"
  for id in $volumes
    set -a cmd $id
  end
  eval $cmd
  continue_prompt
  if test $status -eq 1
    return 1
  end
  manage_volume
end

function manage_network
  set networks (docker network ls | fzf -m --header-lines=1 | awk '{print $2}')
  if test (count $networks) -eq 0
    return 0
  end
  set action (printf 'inspect\nprune\nrm' | fzf --no-info)
  if test -z $action
    manage_network
    if test $status -eq 1
      return 1
    end
    return 0
  end
  set cmd "docker network $action"
  for id in $networks
    set -a cmd $id
  end
  eval $cmd
  continue_prompt
  if test $status -eq 1
    return 1
  end
  manage_network
end

function hock
  if not systemctl is-active docker > /dev/null
    set choice (printf 'start docker ?\nyes\nno' | fzf --header-lines=1 --no-info)
    if test -z $choice
      return 0
    end
    switch $choice
      case yes
        sudo systemctl start docker
        if test $status -ne 0
          return 1
        end
      case no
        return 0
    end
  end
  set choice (printf 'container\nimage\nvolume\nnetwork\nstop docker\ndocker-compose up -d' | fzf --no-info)
  if test -z $choice
    return 0
  end
  switch $choice
    case container
      manage_container
      if test $status -eq 1
        return 0
      end
      hock
    case image
      manage_image
      if test $status -eq 1
        return 0
      end
      hock
    case volume
      manage_volume
      if test $status -eq 1
        return 0
      end
      hock
    case network
      manage_network
      if test $status -eq 1
        return 0
      end
      hock
    case 'stop docker'
      sudo systemctl stop docker
      return 0
    case 'docker-compose up -d'
      docker-compose up -d
      return 0
  end
end
