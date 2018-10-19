# Defined in /home/pierre/.config/fish/functions/fish_prompt.fish @ line 1
function fish_prompt
	test $SSH_TTY
    and printf (set_color red)$USER(set_color brwhite)'@'(set_color yellow)(prompt_hostname)' '
    test "$USER" = 'root'
    and echo -n (set_color cyan)(prompt_pwd) (set_color red)'❯'(set_color purple)'❯'(set_color blue)'❯ '

    # Main
    echo -n (set_color cyan)(prompt_pwd) (set_color red)'❯'(set_color yellow)'❯'(set_color green)'❯ '
end
