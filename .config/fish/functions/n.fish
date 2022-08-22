# nnn - cd on quit mode

function n --wraps nnn --description 'launch nnn with cd on quit mod'
  # Block nesting of nnn in subshells
  if test -n "$NNNLVL"
    if [ (expr $NNNLVL + 0) -ge 1 ]
      echo "nnn is already running"
      return
    end
  end

  # nnn will write the cd command in this file on quit
  set -x NNN_TMPFILE "$XDG_CONFIG_HOME/nnn/.lastd"

  # the `command` command allows one to alias this function to
  # `nnn` without making an infinitely recursive alias
  command nnn $argv

  if test -e $NNN_TMPFILE
    source $NNN_TMPFILE
    rm $NNN_TMPFILE
  end
end
