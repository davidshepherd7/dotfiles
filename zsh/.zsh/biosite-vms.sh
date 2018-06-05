#!/usr/bin/zsh -f

# zsh config for vm interaction

# Requires you to export boron_dir="..." somewhere before calling some of these functions



_complete-containers() {
    reply=(`docker ps --format "{{.Names}}"`)
}

docker-get-ip() {
    local server_name="$1"
    docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' "$server_name"
}
compctl -K _complete-containers docker-get-ip


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

docker-client() {
    "$boron_dir/scripts/docker/client.sh" "$@"
}
compctl -K _complete-containers docker-client


reset-vm() {
    local vm_name="$1"
    vboxmanage controlvm "$vm_name" poweroff || true
    vboxmanage discardstate "$vm_name" || true
    vboxmanage startvm "$vm_name" --type headless
}
compctl -K _complete-vms reset-vm
