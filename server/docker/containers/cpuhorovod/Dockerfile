from horovod/horovod-cpu
# Switch to root user to install additional software
USER 0


# Make sure we don't get notifications we can't answer during building.
env    DEBIAN_FRONTEND noninteractive

run    apt-get -q -y update; apt-get -q -y upgrade
run    apt install net-tools

# Prepare scripts and configs
add    ./scripts/start /start


# Set root password
run    echo 'root:password' >> /root/passwdfile


# Create user and it's password
run    useradd -m -G sudo master && \
       echo 'master:password' >> /root/passwdfile


# Apply root password
run    chpasswd -c SHA512 < /root/passwdfile && \
       rm /root/passwdfile


# Port 22 is used for ssh
expose 22


# Assign /data as static volume.
volume ["/data"]


# Fix all permissions
run    chmod +x /start


# Starting sshd
cmd    ["/start"]