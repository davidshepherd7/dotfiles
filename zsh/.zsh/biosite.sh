#! /bin/zsh

bgulpl()
{
    # subshell to avoid changing directory
    (
        cd ~/code/boron-unstable/boron/web_applications/;
        gulp --silent --reporter=simple "$@"
    )
}

alias bgulp='bgulpl --nolint'

alias lbiosite='/home/david/code/boron-unstable/scripts/launch-biosite-single-terminal.sh'

bgulpr()
{
    bgulp "$@" && refresh-browser.sh
}

bgulplr()
{
    bgulpl "$@" && refresh-browser.sh
}

aburl()
{
    burl "$1" \
         -H 'Authorization: Bearer eyJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJyb290In0.KeGNGBnBlQ5aiWv_3k1tMuxySXQwaEwbkb__Dqgu9T8' \
         "${@:2}"
}

acurl() {
    curl "$1" \
         -k \
         -H 'Authorization: Bearer eyJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJyb290In0.KeGNGBnBlQ5aiWv_3k1tMuxySXQwaEwbkb__Dqgu9T8' \
         "${@:2}"
}

burl()
{
    if [[ "$#" < 1 ]] || [[ "$1" = "-h" ]]; then
        echo "Usage: bcurl [boron_api_server_path] [curl opts]" 1>&2
        return 1
    fi

    curl \
        -k  \
        "https://localhost:8088${1}" \
        "${@:2}"
}

alias boron-client='/home/david/code/boron-unstable/build/bin/boron-client'


# Get the ip of a local virtualbox vm. The VM must be running, booted into
# Linux, have the package virtualbox-guest-utils installed, and have been
# rebooted since that package was installed.
get-vm-ip () {
    server_name="$1"
    vboxmanage guestproperty get "$server_name" '/VirtualBox/GuestInfo/Net/0/V4/IP' \
        | awk '{print $2}'
}

# ssh to a local virtualbox vm. The same caveats as with get-vm-ip apply.
ssh-vm () {
    ssh "$(get-vm-ip "$1")" -o StrictHostKeyChecking=no "${@:2}"
}

# TODO: try this out
_complete-vms() {
    reply=(`vboxmanage list runningvms`);
}
compctl -K _complete-vms get-vm-ip
compctl -K _complete-vms ssh-vm


cd-boron-build () {
    cd '/home/david/code/boron-unstable/build'
}

boron-test () {
    (
        cd-boron-build && ninja "$1" && "./bin/$1" "$2"
    )
}
_boron-test () {
    reply=(`cd-boron-build && ninja -t targets | awk -F: '{print $1}' | grep 'test'`);
}
compctl -K _boron-test boron-test
alias esfix="eslint -c ~/code/boron-unstable/boron/web_applications/future-eslintrc.json --fix"
