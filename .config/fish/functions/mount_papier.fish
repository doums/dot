function mount_papier
    sshfs papier_fs:/mnt/us /opt/mnt/papier -C -o reconnect,idmap=user,cache_timeout=3600
    cd /opt/mnt/papier/
end
