#!/usr/bin/zsh -f

# zsh config for vm interaction

# Requires you to export boron_dir="..." somewhere before calling some of these functions



# Auto completion for commands which take a vm name
_complete-vms() {
    reply=(`vboxmanage list runningvms | awk '{print $1}' | tr -d '"'`);
}

_complete-containers() {
    reply=(`docker ps --format "{{.Names}}"`)
}

# Get the ip of a local virtualbox vm. The VM must be running, booted into
# Linux, have the package virtualbox-guest-utils installed, and have been
# rebooted since that package was installed.
vm-get-ip() {
    local server_name="$1"
    vboxmanage guestproperty get "$server_name" '/VirtualBox/GuestInfo/Net/0/V4/IP' | awk '{print $2}'
}
compctl -K _complete-vms vm-get-ip

docker-get-ip() {
    local server_name="$1"
    docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' "$server_name"
}
compctl -K _complete-containers docker-get-ip

# As above, but wait until we get an ip
vm-blocking-get-ip() {
    while [[ "$(vm-get-ip "$1")" = "value" ]]; do
        sleep 1
    done
    vm-get-ip "$1"
}
compctl -K _complete-vms vm-blocking-get-ip


# ssh to a local virtualbox vm. The same caveats as with get-vm-ip apply.
vm-ssh() {
    local vm_name="$1"
    shift 1

    # StrictHostKeyChecking=no disables warnings about it being a new server.
    \ssh "boron-vm@$(vm-blocking-get-ip "$vm_name")" \
         -o StrictHostKeyChecking=no \
         -t -t tmux new-session "$@"
}
compctl -K _complete-vms vm-ssh

vm-scp() {
    local vm_name="$1"
    local file_name="$2"
    shift 1

    # StrictHostKeyChecking=no disables warnings about it being a new server.
    \scp -o StrictHostKeyChecking=no "$file_name" "boron-vm@$(vm-blocking-get-ip "$vm_name"):~/"

}
compctl -K _complete-vms vm-scp

vm-psql() {
    local vm_name="$1"
    shift 1

    vm-ssh "$vm_name" env PGPASSWORD="biosite" psql -U boron_user boron -h localhost "$@"
}
compctl -K _complete-vms vm-psql

docker-psql() {
    local docker_name="$1"
    shift 1

    docker exec -it "$docker_name" \
           env PGPASSWORD="biosite" \
           psql -U boron_user boron -h localhost "$@"
}
compctl -K _complete-containers docker-psql

# Shutdown, delete, and clear data for a VM
nuke-vm() {
    local vm_name="$1"
    (cd "$boron_dir" && ./scripts/vm-management/nuke-vm.sh "$vm_name")
}
compctl -K _complete-vms nuke-vm
