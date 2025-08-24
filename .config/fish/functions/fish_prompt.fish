function fish_prompt
    set -l __last_command_exit_status $status

    set -l red (set_color -o red)
    set -l white (set_color -o white)
    set -l normal (set_color normal)

    set -l arrow_color "$white"
    if test $__last_command_exit_status != 0
        set arrow_color "$red"
    end

    set -l arrow "$arrow_color› "
    if fish_is_root_user
        set arrow "$arrow_color# "
    end

    set -l cwd $normal(prompt_pwd)

    echo -n -s $arrow ' '$cwd $normal ' '
end
