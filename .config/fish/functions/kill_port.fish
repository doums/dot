# Defined in - @ line 2
function kill_port
	fuser -k $argv[1]/tcp
end
