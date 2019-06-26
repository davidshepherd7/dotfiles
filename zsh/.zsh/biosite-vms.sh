#!/usr/bin/zsh -f

# zsh config for vm interaction

# Requires you to export boron_dir="..." somewhere before calling some of these functions



_complete-containers() {
    reply=(`docker ps --format "{{.Names}}"`)
}

_complete-vms() {
    reply=(`vboxmanage list runningvms | awk '{print $1}' | tr -d '"' | tr "\n" " "`)
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
           psql -U boron_user boron "$@"
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

vm-send-binary() {
    local vm_name="$1"
    local binary_name="$2"

    local binary_basename="$(basename "$(readlink "$binary_name")")"
    echo "basename: $binary_basename"

    scp -o StrictHostKeyChecking=no "$binary_name" "boron-vm@$(vm-blocking-get-ip "$vm_name"):/home/boron-vm/$binary_basename"
    ssh "boron-vm@$(vm-blocking-get-ip "$vm_name")" -o StrictHostKeyChecking=no <<EOF
target=\$(echo /opt/BioSite/current/$binary_basename)
sudo mv "/home/boron-vm/$binary_basename" "\$target"
sudo chrpath "\$target" -r "\\$ORIGIN:/opt/BioSite/current"
EOF
}


reset-vm() {
    local vm_name="$1"
    vboxmanage controlvm "$vm_name" poweroff || true
    vboxmanage discardstate "$vm_name" || true
    vboxmanage startvm "$vm_name" --type headless
}
compctl -K _complete-vms reset-vm
